rm(list=ls(all=T))
require(quantmod)
require(TTR)
require('dygraphs')
require('lubridate')
require('dplyr')

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/help')

pricedata = readSHindex()

cl = Cl(pricedata) - Op(pricedata)
clflag = ifelse(cl < 0 ,'down','up')
leadclflag = shift(clflag,n=1,type='lead')
analysedata = merge(leadclflag,clflag)

smashort =SMA(Cl(pricedata),n=5)
smalong =SMA(Cl(pricedata),n=3)
smasignal = (function(smalong,smashort){
  smashort$preshort = lag(smashort,1)
  smalong$prelong = lag(smalong,1)
  signal = ifelse( smashort$preshort < smalong$prelong & smashort$SMA >= smalong$SMA,'long',
               ifelse(smashort$preshort > smalong$prelong & smashort$SMA <= smalong$SMA,'short','hold'))
  return(signal)
  })(sma20,sma3)
  
analysedata$smasignal = smasignal

cci = CCI(HLC(pricedata),n=5)
ccisignal = ifelse(cci > 100,'long',ifelse(cci< -100,'short','hold'))
analysedata$ccisignal = ccisignal

rsi = RSI(Cl(pricedata),n=5)
rsisignal = ifelse(rsi > 70,'short',ifelse(rsi< 30,'long','hold'))
analysedata$rsisignal = rsisignal


macd = MACD(Cl(pricedata))
macdsignal = (function(macd){
  macd$premacd = lag(macd$macd,1)
  signal = ifelse(macd$premacd < 0 & macd$macd >0,'long',ifelse(macd$premacd > 0 & macd$macd<0,'short','hold'))
  return(signal)
  })(macd)
analysedata$macdsignal = macdsignal

adx = ADX(HLC(pricedata))
adxsignal = (function(adx)
  {
    adx$predip = lag(adx$DIp,n=1)
    adx$predin = lag(adx$DIn,n=1)
    signal = ifelse(adx$predip < adx$predin & adx$DIp > adx$DIn,'long',ifelse(adx$predip < adx$predin & adx$DIn > adx$DIp,'short','hold'))
    return(signal)
    })(adx)
analysedata$adxsignal = adxsignal

mfi = MFI(HLC(pricedata),Vo(pricedata),n=5)
mfisignal = (function(mfi){
  mfi$premfi = lag(mfi,1)
  signal = ifelse(mfi$premfi < 20 & mfi$mfi >= 20,'long',ifelse(mfi$premfi > 80 & mfi$mfi <=80,'short','hold'))
  return(signal)
  })(mfi)
analysedata$mfisignal = mfisignal


# prepare dataset
analysedata_train = as.data.frame(analysedata['1995/2014'])
analysedata_train = na.omit(analysedata_train)

analysedata_test = as.data.frame(analysedata['2015'])
analysedata_test = na.omit(analysedata_test)

#naiveBayes
model = naiveBayes(leadclflag ~ .,data=analysedata_train,laplace=1)

pr = predict(model,analysedata_test,type = 'raw')

predictvalue = ifelse(pr[,1] > 0.55 ,'down',ifelse(pr[,2] > 0.55,'up','unkown'))
 
judege = data.frame(leadflag = analysedata_test[,1],predict = predictvalue)
result =apply(judege, MARGIN = 1, FUN=function(x){
 if(x[1] == x[2])
 {
   return('y')
 }
  if(x[1] != x[2] && x[2] != 'unkown')
  {
    return('n')
  }
  else
  {
    return('unkown')
  }
}
  )

length(result)
sum(result=='y')
sum(result == 'n')
