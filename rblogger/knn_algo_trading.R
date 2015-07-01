#http://gekkoquant.com/2013/12/02/k-nearest-neighbour-algo-fail/
#http://gekkoquant.com/2014/01/26/k-nearest-neighbour-algo-find-closest-period-in-history/

library("quantmod")
library("PerformanceAnalytics")
library("zoo")

#INPUTS
marketSymbol <- "ARM.L"

nFastLookback <- 30 #The fast signal lookback used in linear regression curve
nSlowLookback <- 50 #The slow signal lookback used in linear regression curve

nFastVolLookback <- 30 #The fast signal lookback used to calculate the stdev
nSlowVolLookback <- 50 #The slow signal lookback used calculate the stdev

nFastRSILookback <- 30 #The fast signal lookback used to calculate the stdev
nSlowRSILookback <- 50 #The slow signal lookback used calculate the stdev

kNearestGroupSize <- 50 #How many neighbours to use
normalisedStrengthVolWeight <- 2 #Make some signals more important than others in the MSE
normalisedStrengthRegressionWeight <- 1
fastRSICurveWeight <- 2
slowRSICurveWeight <- 0.8


#Specify dates for downloading data, training models and running simulation
startDate = as.Date("2006-08-01") #Specify what date to get the prices from
symbolData <- new.env() #Make a new environment for quantmod to store data in

stockCleanNameFunc <- function(name){
  return(sub("^","",name,fixed=TRUE))
}

getSymbols(marketSymbol, env = symbolData, src = "yahoo", from = startDate)
cleanName <- stockCleanNameFunc(marketSymbol)
mktData <- get(cleanName,symbolData)

linearRegressionCurve <- function(data,n){
  regression <- function(dataBlock){
    fit <-lm(dataBlock~seq(1,length(dataBlock),1))
    return(last(fit$fitted.values))
  }
  return (rollapply(data,width=n,regression,align="right",by.column=FALSE,na.pad=TRUE))
}

volCurve <- function(data,n){
  stdev <- function(dataBlock){
    sd(dataBlock)
  }
  return (rollapply(data,width=n,stdev,align="right",by.column=FALSE,na.pad=TRUE))^2
}

fastRegression <- linearRegressionCurve(Cl(mktData),nFastLookback)
slowRegression <- linearRegressionCurve(Cl(mktData),nSlowLookback)
normalisedStrengthRegression <- slowRegression / (slowRegression+fastRegression)

fastVolCurve <- volCurve(Cl(mktData),nFastVolLookback)
slowVolCurve <- volCurve(Cl(mktData),nSlowVolLookback)
normalisedStrengthVol <- slowVolCurve / (slowVolCurve+fastVolCurve)

fastRSICurve <-RSI(Cl(mktData),nFastRSILookback)/100 #rescale it to be in the same range as the other indicators
slowRSICurve <-RSI(Cl(mktData),nSlowRSILookback)/100

#Lets plot the signals just to see what they look like
dev.new()
par(mfrow=c(2,2))
plot(normalisedStrengthVol,type="l")
plot(normalisedStrengthRegression,type="l")
plot(fastRSICurve,type="l")
plot(slowRSICurve,type="l")



#DataMeasure will be used to determine how similar other days are to today
#It is used later on for calculate the days which are most similar to today according to MSE measure
dataMeasure <- cbind(normalisedStrengthVol*normalisedStrengthVolWeight,normalisedStrengthRegression*normalisedStrengthRegression,fastRSICurve*fastRSICurveWeight,slowRSICurve*slowRSICurveWeight)
colnames(dataMeasure) <- c("normalisedStrengthVol","normalisedStrengthRegression","fastRSICurve","slowRSICurve")

#Finds the nearest neighbour and calculates the trade signal
calculateNearestNeighbourTradeSignal <- function(dataMeasure,K,mktReturns){
  findKNearestNeighbours <- function(dataMeasure,K){
    calculateMSE <- function(dataMeasure){
      calculateMSEInner <- function(dataA,dataB){
        se <- ((as.matrix(dataA) - as.matrix(dataB))^2)
        apply(se,1,mean)
      }
      
      #Repeat the last row of dataMeasure multiple times
      #This is so we can compare dataMeasure[today] with all the previous dates
      lastMat <- last(dataMeasure)
      setA <-  lastMat[rep(1, length(dataMeasure[,1])),]
      setB <- dataMeasure
      
      mse <- calculateMSEInner(setB,setA)
      mse[is.na(mse)] <- Inf #Give it a terrible MSE if it's NA
      colName <-  c(colnames(dataMeasure),"MSE")
      dataMeasure <- cbind(dataMeasure,mse)
      colnames(dataMeasure) <- colName
      return (dataMeasure)
    }
    
    rowNum <- seq(1,length(dataMeasure[,1]),1)
    dataMeasureWithMse <- as.data.frame(calculateMSE(dataMeasure))
    tmp <- c("rowNum", colnames(dataMeasureWithMse))
    dataMeasureWithMse <- cbind(rowNum,dataMeasureWithMse)
    colnames(dataMeasureWithMse) <- tmp
    dataMeasureWithMse <- dataMeasureWithMse[order(dataMeasureWithMse[,"MSE"]),]
    #Starting from the 2nd item as the 1st is the current day (MSE will be 0) want to drop it
    return (dataMeasureWithMse[seq(2,min(K,length(dataMeasureWithMse[,1]))),])
  }
  
  calculateTradeSignalFromKNeighbours <- function(mktReturns,kNearestNeighbours){
    rowNums <- kNearestNeighbours[,"rowNum"]
    rowNums <- na.omit(rowNums)
    if(length(rowNums) <= 1) { return (0) }
    print("The kNearestNeighbours are:")
    print(rowNums)
    
    #So lets see what happened on the day AFTER our nearest match
    mktRet <- mktReturns[rowNums+1]
    
    #return (sign(sum(mktRet)))
    return (SharpeRatio.annualized(mktRet))
  }
  
  kNearestNeighbours <- findKNearestNeighbours(dataMeasure,K)
  tradeSignal <- calculateTradeSignalFromKNeighbours(mktReturns,kNearestNeighbours)
  return(tradeSignal)
  
}

ret <- (Cl(mktData)/Op(mktData))-1
signalLog <- as.data.frame(ret)
signalLog[,1] <- 0
colnames(signalLog) <- c("TradeSignal")

#Loop through all the days we have data for, and calculate a signal for them using nearest neighbour
for(i in seq(1,length(ret))){
  print (paste("Simulating trading for day",i,"out of",length(ret),"@",100*i/length(ret),"%"))
  index <- seq(1,i)
  signal <- calculateNearestNeighbourTradeSignal(dataMeasure[index,],kNearestGroupSize,ret)
  signalLog[i,1] <- signal
}

dev.new()
tradeRet <- Lag(signalLog[,1])*ret[,1] #Combine todays signal with tomorrows return (no lookforward issues)
totalRet <- cbind(tradeRet,ret)
colnames(totalRet) <- c("Algo",paste(marketSymbol," Long OpCl Returns"))
charts.PerformanceSummary(totalRet,main=paste("K nearest trading algo for",marketSymbol),geometric=FALSE)
print(SharpeRatio.annualized(tradeRet))