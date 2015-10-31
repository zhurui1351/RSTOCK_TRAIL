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

#引入辅助函数
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/help')

#读入数据
pricedata = readSHindex()
#处理待预测数据，用lead提前一天，把头天的涨跌加入预测变量
cl = Cl(pricedata) - Op(pricedata)
clflag = ifelse(cl < 0 ,'down','up')
leadclflag = shift(clflag,n=1,type='lead')
analysedata = merge(leadclflag,clflag)

#添加指标集合
smashort =SMA(Cl(pricedata),n=5)
smalong =SMA(Cl(pricedata),n=3)
smasignal = (function(smalong,smashort){
  smashort$preshort = lag(smashort,1)
  smalong$prelong = lag(smalong,1)
  signal = ifelse( smashort$preshort < smalong$prelong & smashort$SMA >= smalong$SMA,'long',
               ifelse(smashort$preshort > smalong$prelong & smashort$SMA <= smalong$SMA,'short','hold'))
  return(signal)
  })(smalong,smashort)
  
analysedata$smasignal = smasignal

cci = CCI(HLC(pricedata),n=5)
ccisignal = (function(cci){
  cci$precci = lag(cci,1)
  ccisignal = ifelse((cci$precci < 100 & cci$cci > 100) | (cci$precci < -100 & cci$cci > -100),'long',
                     ifelse((cci$precci > 100 & cci$cci < 100) | (cci$precci > -100 & cci$cci < -100),'short','hold'))
  return(ccisignal)
})(cci)
 analysedata$ccisignal = ccisignal

rsi = RSI(Cl(pricedata),n=5)
rsisignal = (function(rsi){
  rsi$prersi = lag(rsi,1)
  rssignal = ifelse(rsi$prersi < 70 & rsi$EMA > 70,'long',
                    ifelse(rsi$prersi > 30 & rsi$EMA < 30,'short','hold'))
  return(rssignal)
})(rsi)
analysedata$rsisignal = rsisignal


macd = MACD(Cl(pricedata))
macdsignal = (function(macd){
  macd$premacd = lag(macd$macd,1)
  signal = ifelse(macd$premacd < 0 & macd$macd >0,'long',ifelse(macd$premacd > 0 & macd$macd<0,'short','hold'))
  return(signal)
  })(macd)
analysedata$macdsignal = macdsignal

adx = ADX(HLC(pricedata),n=5)
adxsignal = (function(adx)
  {
    adx$predip = lag(adx$DIp,n=1)
    adx$predin = lag(adx$DIn,n=1)
    signal = ifelse(adx$predip < adx$predin & adx$DIp > adx$DIn,'long',ifelse(adx$predin < adx$predip & adx$DIn > adx$DIp,'short','hold'))
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

aro = aroon(HLC(pricedata)[,c(1,2)],n=5)
arosignal = (function(aro){
  aro$preos = lag(aro$oscillator,1)
  signal = ifelse(aro$preos < 0 & aro$oscillator > 0,'long',
                  ifelse(aro$preos > 0 & aro$oscillator < 0,'short','hold'))
  return(signal)
})(aro)
analysedata$arosignal = arosignal

bbands = BBands(HLC(pricedata),n=5)
bbandssignal = (function(bbands,Close){
  bdata = merge(bbands,Close)
  bdata$precl = lag(bdata$Close,1)
  bdata$preup = lag(bdata$up,1)
  bdata$predn =lag(bdata$dn,1)
  signal = ifelse(bdata$precl > bdata$predn & bdata$Close < bdata$dn,'long',
                  ifelse(bdata$precl < bdata$preup & bdata$Close > bdata$up,'short','hold'))
  return(signal)
})(bbands,Cl(pricedata))
analysedata$bbandssignal = bbandssignal

roc = ROC(Cl(pricedata),n=5)
rocsignal = (function(roc){
  roc$pre = lag(roc,1)
  signal = ifelse(roc$pre < 0 & roc$Close >0,'long',ifelse(roc$pre > 0 & roc$Close < 0 ,'short','hold'))
  return(signal)
})(roc)
analysedata$rocsignal = rocsignal

sar = SAR(HLC(pricedata)[,c(1,2)])
sarsignal = (function(sar,Close){
  sardata = merge(sar,Close)
  sardata$precl = lag(sardata$Close,1)
  sardata$presar = lag(sardata$sar,1)
  signal = ifelse(sardata$precl < sardata$presar & sardata$Close > sardata$sar,'long',
                     ifelse(sardata$precl > sardata$presar & sardata$Close < sardata$sar,'short','hold'))
  return(signal)
})(sar,Cl(pricedata))
analysedata$sarsignal = sarsignal

wpr = WPR(HLC(pricedata),n=10)
wprsignal = (function(wpr){
  wpr$pre = lag(wpr$Close,1)
  signal = ifelse(wpr$pre < 0.8 & wpr$Close > 0.8,'long',ifelse(wpr$pre > 0.2 & wpr$Close < 0.2,'short','hold'))
  return(signal)
})(wpr)
analysedata$wprsignal = wprsignal

kdj = stoch(HLC(pricedata), nFastK = 10, nFastD = 3, nSlowD = 3)
kdjsignal = (function(x){
  kdj$prefastk = lag(kdj$fastK,1)
  kdj$prefastd = lag(kdj$fastD,1)
  signal = ifelse( kdj$prefastk < kdj$prefastd & kdj$fastK > kdj$fastD,'long',
                   ifelse(kdj$prefastk > kdj$prefastd & kdj$fastK < kdj$fastD,'short','hold'))
  return(signal)
})(kdj)
analysedata$kdjsignal = kdjsignal

smi = SMI(HLC(pricedata), n = 13, nFast = 2, nSlow = 10)
simsignal = (function(smi){
  smi$pre = lag(smi$SMI,1)
  signal = ifelse(smi$pre > 20 & smi$SMI < 20,'long',ifelse(smi$pre < 80 & smi$SMI > 80,'short','hold'))
  return(signal)
})(smi)

analysedata$simsignal = simsignal

tdi = TDI(OHLC(pricedata),n=10)
tdisignal = (function(tdi){
  tdi$pretdi = lag(tdi$tdi,1)
  signal = ifelse(tdi$pretdi < 0 & tdi$tdi > 0,'long',ifelse(tdi$pretdi > 0 & tdi$tdi < 0,'short','hold'))
  return(signal)
})(tdi)
analysedata$tdisignal = tdisignal

kst = KST(Cl(pricedata))
kstsignal = (function(kst){
  kst$sma = SMA(kst$kst,n=5)
  kst$prekst = lag(kst$kst,1)
  kst$presma = lag(kst$sma,1)
  signal = ifelse(kst$prekst < kst$presma & kst$kst > kst$sma,'long',
                  ifelse(kst$prekst > kst$presma & kst$kst < kst$sma,'short','hold'))
  return(signal)
})(kst)
analysedata$kstsignal = kstsignal

chaikinad = chaikinAD(HLC(pricedata),Vo(pricedata))
chaikinadsignal = (function(chaikinad,close){
  chaikdata = merge(close,chaikinad)
  chaikdata$precl = lag(chaikdata$Close,1)
  chaikdata$prech = lag(chaikdata$chaikinad,1)
  signal = ifelse((chaikdata$Close - chaikdata$precl) < 0 & (chaikdata$chaikinad - chaikdata$prech ) > 0,'long',
                  ifelse((chaikdata$Close - chaikdata$precl) > 0 & (chaikdata$chaikinad - chaikdata$prech ) < 0,'short','hold'))
  return(signal)
})(chaikinad,Cl(pricedata))
analysedata$chaikinadsignal = chaikinadsignal

obv = OBV(Cl(pricedata),Vo(pricedata))
obvsignal = (function(obv,close){
  obvdata = merge(obv,close)
  names(obvdata) = c('obv','Close')
  obvdata$preobv = lag(obvdata$obv,1)
  signal = ifelse(obvdata$Close < 0 & (obvdata$obv - obvdata$preobv) > 0 ,'long',
                  ifelse(obvdata$Close > 0 & (obvdata$obv - obvdata$preobv) < 0 ,'short','hold'))
  return(signal)
})(obv,OpCl(pricedata))
analysedata$obvsignal = obvsignal

cmo = CMO(Cl(pricedata),n=10)
cmosignal = (function(cmo){
  names(cmo) = 'cmo'
  cmo$sma = SMA(cmo,5)
  cmo$precmo = lag(cmo$cmo,1)
  cmo$presma = lag(cmo$sma,1)
  signal = ifelse(cmo$precmo < cmo$presma & cmo$cmo > cmo$sma,'long',ifelse(cmo$precmo > cmo$presma & cmo$cmo < cmo$sma,'short','hold'))
  return(signal)
})(cmo)
analysedata$cmosignal = cmosignal

cmf = CMF(HLC(pricedata),Vo(pricedata),n=10)
cmfsignal = (function(cmf){
  names(cmf) = 'cmf'
  cmf$precmf = lag(cmf$cmf,1)
  signal = ifelse( cmf$precmf < 0 & cmf$cmf > 0,'long',ifelse(cmf$precmf > 0 & cmf$cmf < 0,'short','hold'))
  return(signal)
})(cmf)
analysedata$cmfsignal = cmfsignal

emv = EMV(HLC(pricedata)[,c(1,2)],Vo(pricedata),n=9)
emvsignal = (function(emv){
  emv$preemv = lag(emv$emv,1)
  signal = ifelse(emv$preemv < 0 & emv$emv > 0,'long',ifelse(emv$preemv > 0 & emv$emv < 0,'short','hold'))
  return(signal)
})(emv)  
analysedata$emvsignal = emvsignal

trix = TRIX(Cl(pricedata),n = 10, nSig = 5)
trixsignal = (function(trix){
  trix$pretrix = lag(trix$TRIX,1)
  trix$presignal = lag(trix$signal,1)
  signal = ifelse(trix$pretrix < trix$presignal & trix$TRIX > trix$signal,'long',
                  ifelse(trix$pretrix > trix$presignal & trix$TRIX < trix$signal,'short','hold'))
  return(signal)
})(trix)
analysedata$trixsignal = trixsignal

williamsad = williamsAD(OHLC(pricedata)[,c(1,2,4)])
williamsadsignal = (function(williamsad,close){
  willdata = merge(williamsad,close)
  names(willdata) = c('will','cl')
  willdata$prewill = lag(willdata$will,1)
  signal = ifelse(willdata$cl < 0 & (willdata$will - willdata$prewill) > 0,'long',
                  ifelse(willdata$cl > 0 & (willdata$will - willdata$prewill) < 0,'short','hold'))
  return(signal)
})(williamsad,OpCl(pricedata))
analysedata$williamsadsignal = williamsadsignal

# 划分训练集和测试集
analysedata_train = as.data.frame(analysedata['1995/2008'])
analysedata_train = na.omit(analysedata_train)
analysedata_train[analysedata_train == 'hold'] = NA

analysedata_test = as.data.frame(analysedata['2009'])
analysedata_test = na.omit(analysedata_test)
analysedata_test[analysedata_test == 'hold'] = NA

#建模
model = naiveBayes(leadclflag ~ . - Close,
                   data=analysedata_train,na.action = na.pass)

pr = predict(model,analysedata_test,type = 'raw')

predictvalue = ifelse(pr[,1] > 0.6 ,'down',ifelse(pr[,2] > 0.6,'up','unkown'))
 
table(analysedata_test[,1],predictvalue)

index_num = c()
 for(i in 1 : nrow(analysedata_test))
 {
    rowdata = analysedata_test[i,]
    if(sum(!is.na(rowdata)) > 3)
      index_num = c(index_num,i)
 }
#analysedata_test[index_num,]
pr = predict(model,analysedata_test[index_num,],type = 'raw')
predictvalue = ifelse(pr[,1] > 0.55 ,'down',ifelse(pr[,2] > 0.55,'up','unkown'))

table(analysedata_test[index_num,1],predictvalue)


#测试,产生交易记录 
#每年更新模型


#每个月更新模型
