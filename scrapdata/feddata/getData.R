#data source https://research.stlouisfed.org/fred2/
#http://gekkoquant.com/2014/05/05/economic-data-in-r-nonfarms-payroll-other-fed-data/

library("quantmod")

#To see what the datasets are available from the FED goto the link below
#http://research.stlouisfed.org/fred2/

economicData <- new.env() #Make a new environment for quantmod to store data in

startDate = as.Date("1900-01-01") #Specify what date to get the prices from
getSymbols("PAYEMS",src="FRED",env=economicData,from=startDate) #Payems is non-farms payrolls
getSymbols("^GSPC",env=economicData,from=startDate) #S&P 500



economicData$PAYEMS <- window(economicData$PAYEMS,start=startDate) #Window our data (FRED ignores the from parameter above) :@
economicData$GSPC <- window(economicData$GSPC,start=startDate) #Window our data

mergedData <- merge(economicData$PAYEMS,Cl(economicData$GSPC),all=FALSE) #join the two datasets based on their SHARED dates

#Calculate the % diff
mergedPercDiff<- mergedData
mergedPercDiff$PAYEMS <- diff(mergedData$PAYEMS)/Lag(mergedData$PAYEMS)
mergedPercDiff$GSPC.Close <- diff(mergedData$GSPC.Close)/Lag(mergedData$GSPC.Close)

dev.new()
par(mfrow=c(2,2))
plot(mergedData$PAYEMS, main="Non-Farm Payrolls",ylab="Thousands of Persons")
plot(mergedPercDiff$PAYEMS, main="Non-Farm Payrolls", ylab="% Change")
plot(mergedData$GSPC.Close, main="S&P 500 Close",ylab="Close Price")
plot(mergedPercDiff$GSPC.Close, main="&P 500 Close",ylab="% Change")

#Function to plot data and add regression line
doPlot <- function(x,y,title,xlabel,ylabel){
  x<-as.vector(x)
  y<-as.vector(y)
  regression <- lm(y~x)
  print(regression)
  plot(y~x,main=title, xlab=xlabel,ylab=ylabel)
  abline(regression,col="red",lwd=1.5)
  legend("bottomleft",paste("y=",regression$coefficients[2],"x+",regression$coefficients[1],sep=""),bg="lightblue")
}

dev.new()
par(mfrow=c(1,2))
doPlot(mergedPercDiff$PAYEMS,mergedPercDiff$GSPC.Close,"Regress Non-Farms Payroll with S&P Monthly Returns","Non-Farms Monthly % Change","S&P 500 Monthly % Change")
doPlot(Lag(mergedPercDiff$PAYEMS),mergedPercDiff$GSPC.Close,"Regress Non-Farms Payroll with NEXT MONTH'S S&P Monthly Return","Non-Farms Monthly % Change","S&P 500 Monthly % Change")
