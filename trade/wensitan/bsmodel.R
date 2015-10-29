rm(list=ls(all=T))
require('quantmod')
require('TTR')
require('dygraphs')
require('lubridate')
require('dplyr')
require('data.table')
require('e1071')
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

smashort =SMA(Cl(pricedata),n=10)
smalong =SMA(Cl(pricedata),n=5)
smasignal = (function(smalong,smashort){
  smashort$preshort = lag(smashort,1)
  smalong$prelong = lag(smalong,1)
  signal = ifelse( smashort$preshort < smalong$prelong & smashort$SMA >= smalong$SMA,'long',
               ifelse(smashort$preshort > smalong$prelong & smashort$SMA <= smalong$SMA,'short','hold'))
  return(signal)
  })(smalong,smashort)
  
analysedata$smasignal = smasignal

cci = CCI(HLC(pricedata),n=10)
ccisignal = ifelse(cci > 100,'long',ifelse(cci< -100,'short','hold'))
analysedata$ccisignal = ccisignal

rsi = RSI(Cl(pricedata),n=10)
rsisignal = ifelse(rsi > 70,'short',ifelse(rsi< 30,'long','hold'))
analysedata$rsisignal = rsisignal


macd = MACD(Cl(pricedata))
macdsignal = (function(macd){
  macd$premacd = lag(macd$macd,1)
  signal = ifelse(macd$premacd < 0 & macd$macd >0,'long',ifelse(macd$premacd > 0 & macd$macd<0,'short','hold'))
  return(signal)
  })(macd)
analysedata$macdsignal = macdsignal

adx = ADX(HLC(pricedata),n=10)
adxsignal = (function(adx)
  {
    adx$predip = lag(adx$DIp,n=1)
    adx$predin = lag(adx$DIn,n=1)
    signal = ifelse(adx$predip < adx$predin & adx$DIp > adx$DIn,'long',ifelse(adx$predip < adx$predin & adx$DIn > adx$DIp,'short','hold'))
    return(signal)
    })(adx)
analysedata$adxsignal = adxsignal

mfi = MFI(HLC(pricedata),Vo(pricedata),n=10)
mfisignal = (function(mfi){
  mfi$premfi = lag(mfi,1)
  signal = ifelse(mfi$premfi < 20 & mfi$mfi >= 20,'long',ifelse(mfi$premfi > 80 & mfi$mfi <=80,'short','hold'))
  return(signal)
  })(mfi)
analysedata$mfisignal = mfisignal


# prepare dataset
analysedata_train = as.data.frame(analysedata['1995/2007'])
analysedata_train = na.omit(analysedata_train)
analysedata_train[analysedata_train == 'hold'] = NA

analysedata_test = as.data.frame(analysedata['2008'])
analysedata_test = na.omit(analysedata_test)
analysedata_test[analysedata_test == 'hold'] = NA

#naiveBayes
model = naiveBayes(leadclflag ~ .,
                   data=analysedata_train,na.action = na.pass)

pr = predict(model,analysedata_test,type = 'raw')

predictvalue = ifelse(pr[,1] > 0.55 ,'down',ifelse(pr[,2] > 0.55,'up','unkown'))
 
table(analysedata_test[,1],predictvalue)

index_num = c()
 for(i in 1 : nrow(analysedata_test))
 {
    rowdata = analysedata_test[i,]
    if(sum(is.na(rowdata)) < 4)
      index_num = c(index_num,i)
 }
#analysedata_test[index_num,]
pr = predict(model,analysedata_test[index_num,],type = 'raw')
predictvalue = ifelse(pr[,1] > 0.55 ,'down',ifelse(pr[,2] > 0.55,'up','unkown'))

table(analysedata_test[index_num,1],predictvalue)
