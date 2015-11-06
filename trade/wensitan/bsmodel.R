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
pricedata = na.omit(pricedata)
#处理待预测数据，用lead提前一天，把头天的涨跌加入预测变量
cl = Cl(pricedata) - Op(pricedata)
clflag = ifelse(cl < 0 ,'down','up')
leadclflag = shift(clflag,n=1,type='lead')
analysedata = merge(leadclflag,clflag)

#添加指标集合 3, 12
smashort =SMA(Cl(pricedata),n=9)
smalong =SMA(Cl(pricedata),n=12)
smasignal = (function(smalong,smashort){
  smashort$preshort = lag(smashort,1)
  smalong$prelong = lag(smalong,1)
  #打叉发出信号
  signal = ifelse( smashort$preshort < smalong$prelong & smashort$SMA >= smalong$SMA,'long',
               ifelse(smashort$preshort > smalong$prelong & smashort$SMA <= smalong$SMA,'short','hold'))
  return(signal)
  })(smalong,smashort)
  
analysedata$smasignal = smasignal
# 5, 100 , -100
cci = CCI(HLC(pricedata),n=5)
ccisignal = (function(cci){
  cci$precci = lag(cci,1)
  #向上穿越100 向下穿越-100 发出信号
  ccisignal = ifelse((cci$precci < 80 & cci$cci > 80) | (cci$precci < -100 & cci$cci > -100),'long',
                     ifelse((cci$precci > 80 & cci$cci < 80) | (cci$precci > -100 & cci$cci < -100),'short','hold'))
  return(ccisignal)
})(cci)
 analysedata$ccisignal = ccisignal

rsi = RSI(Cl(pricedata),n=5)
rsisignal = (function(rsi){
  rsi$prersi = lag(rsi,1)
  #向上穿越70发出long信号，向下穿越30发出short信号，其余为hold
  rssignal = ifelse(rsi$prersi < 70 & rsi$EMA > 70,'long',
                    ifelse(rsi$prersi > 30 & rsi$EMA < 30,'short','hold'))
  return(rssignal)
})(rsi)
analysedata$rsisignal = rsisignal


macd = MACD(Cl(pricedata))
macdsignal = (function(macd){
  macd$premacd = lag(macd$macd,1)
  #向上穿越0线，发出long，向下穿越0线，发出short，其余为hold
  signal = ifelse(macd$premacd < 0 & macd$macd >0,'long',ifelse(macd$premacd > 0 & macd$macd<0,'short','hold'))
  return(signal)
  })(macd)
analysedata$macdsignal = macdsignal

adx = ADX(HLC(pricedata),n=5)
adxsignal = (function(adx)
  {
    adx$predip = lag(adx$DIp,n=1)
    adx$predin = lag(adx$DIn,n=1)
    #ip向上穿越in发出long信号， ip向下穿越in 发出short信号，其余为hold
    signal = ifelse(adx$predip < adx$predin & adx$DIp > adx$DIn,'long',ifelse(adx$predin < adx$predip & adx$DIn > adx$DIp,'short','hold'))
    return(signal)
    })(adx)
analysedata$adxsignal = adxsignal

mfi = MFI(HLC(pricedata),Vo(pricedata),n=5)
mfisignal = (function(mfi){
  mfi$premfi = lag(mfi,1)
  #向上穿越20发出弄信号 向下穿越80发出short信号 其余为hold信号
  signal = ifelse(mfi$premfi < 20 & mfi$mfi >= 20,'long',ifelse(mfi$premfi > 80 & mfi$mfi <=80,'short','hold'))
  return(signal)
  })(mfi)
analysedata$mfisignal = mfisignal

aro = aroon(HLC(pricedata)[,c(1,2)],n=5)
arosignal = (function(aro){
  aro$preos = lag(aro$oscillator,1)
  #向上穿越0线发出long信号，向下穿越0线为short信号，其余为hold
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
  #close价格向上穿越dn close价格向下穿越up 发出信号
  signal = ifelse(bdata$precl > bdata$predn & bdata$Close < bdata$dn,'long',
                  ifelse(bdata$precl < bdata$preup & bdata$Close > bdata$up,'short','hold'))
  return(signal)
})(bbands,Cl(pricedata))
analysedata$bbandssignal = bbandssignal

roc = ROC(Cl(pricedata),n=5)
rocsignal = (function(roc){
  roc$pre = lag(roc,1)
  #穿越0线 发出信号
  signal = ifelse(roc$pre < 0 & roc$Close >0,'long',ifelse(roc$pre > 0 & roc$Close < 0 ,'short','hold'))
  return(signal)
})(roc)
analysedata$rocsignal = rocsignal

sar = SAR(HLC(pricedata)[,c(1,2)])
sarsignal = (function(sar,Close){
  sardata = merge(sar,Close)
  sardata$precl = lag(sardata$Close,1)
  sardata$presar = lag(sardata$sar,1)
  #sar下降，close上升，sar上升，close下降发出信号
  signal = ifelse(sardata$precl < sardata$presar & sardata$Close > sardata$sar,'long',
                     ifelse(sardata$precl > sardata$presar & sardata$Close < sardata$sar,'short','hold'))
  return(signal)
})(sar,Cl(pricedata))
analysedata$sarsignal = sarsignal

wpr = WPR(HLC(pricedata),n=10)
wprsignal = (function(wpr){
  wpr$pre = lag(wpr$Close,1)
  #上穿0.8，下穿0.2
  signal = ifelse(wpr$pre < 0.8 & wpr$Close > 0.8,'long',ifelse(wpr$pre > 0.2 & wpr$Close < 0.2,'short','hold'))
  return(signal)
})(wpr)
analysedata$wprsignal = wprsignal

kdj = stoch(HLC(pricedata), nFastK = 10, nFastD = 3, nSlowD = 3)
kdjsignal = (function(x){
  kdj$prefastk = lag(kdj$fastK,1)
  kdj$prefastd = lag(kdj$fastD,1)
  #k线穿越d线发出信号
  signal = ifelse( kdj$prefastk < kdj$prefastd & kdj$fastK > kdj$fastD,'long',
                   ifelse(kdj$prefastk > kdj$prefastd & kdj$fastK < kdj$fastD,'short','hold'))
  return(signal)
})(kdj)
analysedata$kdjsignal = kdjsignal

smi = SMI(HLC(pricedata), n = 13, nFast = 2, nSlow = 10)
simsignal = (function(smi){
  smi$pre = lag(smi$SMI,1)
  #上穿20，下穿80
  signal = ifelse(smi$pre > 20 & smi$SMI < 20,'long',ifelse(smi$pre < 80 & smi$SMI > 80,'short','hold'))
  return(signal)
})(smi)

analysedata$simsignal = simsignal

tdi = TDI(OHLC(pricedata),n=10)
tdisignal = (function(tdi){
  tdi$pretdi = lag(tdi$tdi,1)
  #上下穿越0线
  signal = ifelse(tdi$pretdi < 0 & tdi$tdi > 0,'long',ifelse(tdi$pretdi > 0 & tdi$tdi < 0,'short','hold'))
  return(signal)
})(tdi)
analysedata$tdisignal = tdisignal

kst = KST(Cl(pricedata))
kstsignal = (function(kst){
  kst$sma = SMA(kst$kst,n=5)
  kst$prekst = lag(kst$kst,1)
  kst$presma = lag(kst$sma,1)
  #kst上下穿越均线
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
  #价格降，ad上升 价格升，ad下降
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
  #价格跌，obv上升，价格升，obv下降，发出信号
  signal = ifelse(obvdata$Close < 0 & (obvdata$obv - obvdata$preobv) > 0 ,'long',
                  ifelse(obvdata$Close > 0 & (obvdata$obv - obvdata$preobv) < 0 ,'short','hold'))
  return(signal)
})(obv,OpCl(pricedata))
analysedata$obvsignal = obvsignal

cmo = CMO(Cl(pricedata),n=19)
cmosignal = (function(cmo){
  names(cmo) = 'cmo'
  cmo$sma = SMA(cmo,15)
  cmo$precmo = lag(cmo$cmo,1)
  cmo$presma = lag(cmo$sma,1)
  #上下穿越其均线，发出信号
  signal = ifelse(cmo$precmo < cmo$presma & cmo$cmo > cmo$sma,'long',ifelse(cmo$precmo > cmo$presma & cmo$cmo < cmo$sma,'short','hold'))
  return(signal)
})(cmo)
analysedata$cmosignal = cmosignal

cmf = CMF(HLC(pricedata),Vo(pricedata),n=10)
cmfsignal = (function(cmf){
  names(cmf) = 'cmf'
  cmf$precmf = lag(cmf$cmf,1)
  #穿越0线，发出信号
  signal = ifelse( cmf$precmf < 0 & cmf$cmf > 0,'long',ifelse(cmf$precmf > 0 & cmf$cmf < 0,'short','hold'))
  return(signal)
})(cmf)
analysedata$cmfsignal = cmfsignal

emv = EMV(HLC(pricedata)[,c(1,2)],Vo(pricedata),n=9)
emvsignal = (function(emv){
  emv$preemv = lag(emv$emv,1)
  #上下穿越0线
  signal = ifelse(emv$preemv < 0 & emv$emv > 0,'long',ifelse(emv$preemv > 0 & emv$emv < 0,'short','hold'))
  return(signal)
})(emv)  
analysedata$emvsignal = emvsignal

trix = TRIX(Cl(pricedata),n = 10, nSig = 5)
trixsignal = (function(trix){
  trix$pretrix = lag(trix$TRIX,1)
  trix$presignal = lag(trix$signal,1)
  #上下穿越信号线
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
  #close下降，will上升，close上升，will下降，发出信号
  signal = ifelse(willdata$cl < 0 & (willdata$will - willdata$prewill) > 0,'long',
                  ifelse(willdata$cl > 0 & (willdata$will - willdata$prewill) < 0,'short','hold'))
  return(signal)
})(williamsad,OpCl(pricedata))
analysedata$williamsadsignal = williamsadsignal


#analysedata$weekdays = shift(weekdays(index(analysedata)),n=1,type='lead')
#analysedata$months = months(index(analysedata))

#测试代码  划分训练集和测试集 测试一年的数据
# analysedata_train = as.data.frame(analysedata['1995/2008'])
# analysedata_train = na.omit(analysedata_train)
# analysedata_train[analysedata_train == 'hold'] = NA
# 
# analysedata_test = as.data.frame(analysedata['2009'])
# analysedata_test = na.omit(analysedata_test)
# analysedata_test[analysedata_test == 'hold'] = NA

#建模验证
# model = naiveBayes(leadclflag ~ . - Close,
#                    data=analysedata_train,na.action = na.pass)
# 
# pr = predict(model,analysedata_test,type = 'raw')

#划定概率范围
# predictvalue = ifelse(pr[,1] > 0.55 ,'down',ifelse(pr[,2] > 0.55,'up','unkown'))
#  
# table(analysedata_test[,1],predictvalue)

#当天有多于n个指标时 才进行预测
# nindex = 3
# index_num = c()
#  for(i in 1 : nrow(analysedata_test))
#  {
#     rowdata = analysedata_test[i,]
#     if(sum(!is.na(rowdata)) > nindex)
#       index_num = c(index_num,i)
#  }
# 
# pr = predict(model,analysedata_test[index_num,],type = 'raw')
# predictvalue = ifelse(pr[,1] > 0.55 ,'down',ifelse(pr[,2] > 0.55,'up','unkown'))
# 
# table(analysedata_test[index_num,1],predictvalue)


#回测,产生交易记录 
#每年更新模型，使用前n年的数据
testdate = substr(as.character(index(to.yearly(pricedata['2000/2015']))),1,4)
starttrainyear = as.character(1995)
records = data.frame()
#analysedata_xts = as.xts(analysedata)
for(y in testdate)
{
  print(y)
  endtraindate = as.character(as.numeric(y) - 1)
#  starttrainyear = as.character(as.numeric(y) - 6)
  analysedata_train = as.data.frame(analysedata[paste(starttrainyear,endtraindate,sep='/')])
  #最后一期的数据不参与建模，比如20141231那天是无法知道2015第一个交易日的涨跌
  analysedata_train = analysedata_train[1:(nrow(analysedata_train) - 1),]
  analysedata_train = na.omit(analysedata_train)
  analysedata_train[analysedata_train == 'hold'] = NA
  
  analysedata_test = as.data.frame(analysedata[y])
  analysedata_test = na.omit(analysedata_test)
  analysedata_test[analysedata_test == 'hold'] = NA
  #建模
  model = naiveBayes(leadclflag ~ . - rsisignal - macdsignal - obvsignal - ccisignal,
                     data=analysedata_train,na.action = na.pass)
  #预测
  pr = predict(model,analysedata_test,type = 'raw')
  
  #用新模型计算上一个周期最后一个交易日的预测，并更新到预测表
  # 比如2015年1月1日 更新模型后， 用新模型对2014-12-31的数据进行预测 来进行2015年第一个交易日的决策
  pf = predict(model,tail(analysedata[endtraindate],1),type='raw')
  #更新预测表，避免look ahead bias
  pr = rbind(pf,pr[1:(nrow(pr)-1),])
  
  #生成回测记录
  for(i in 1:nrow(pr))
  {
    pv = pr[i,]
    tradetime = rownames(analysedata_test[i,])
    
    numerindex = analysedata[tradetime][,2:ncol(analysedata)]
    numerindex = sum(numerindex != 'hold')
    if(numerindex < 4) next;
    
    enter = as.numeric(Op(pricedata[tradetime]))
    out = as.numeric(Cl(pricedata[tradetime]))
    if(pv['down'] > 0.55)
    {    
      record = data.frame(code='index',opdate=tradetime,cldate=tradetime,Open=enter,Close=out,profit=as.numeric(out-enter),initStop=0,stopprice=0,type='short')
      records = rbind(records,record)
    }
    if(pv['up'] > 0.55)
    {
      record = data.frame(code='index',opdate=tradetime,cldate=tradetime,Open=enter,Close=out,profit=as.numeric(out-enter),initStop=0,stopprice=0,type='long')
      records = rbind(records,record)
    }
  }
}

bsloganalysis(records)
