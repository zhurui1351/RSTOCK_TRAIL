getSMAsignal = function(pricedata,short,long,period){
  smashort =SMA(Cl(pricedata),n=short)
  smalong =SMA(Cl(pricedata),n=long)
  smashort$preshort = lag(smashort,1)
  smalong$prelong = lag(smalong,1)
  #打叉发出信号
  signal = ifelse( smashort$preshort < smalong$prelong & smashort$SMA >= smalong$SMA,'long',
                   ifelse(smashort$preshort > smalong$prelong & smashort$SMA <= smalong$SMA,'short','hold'))
  return(signal[period])
}


getCCIsignal = function(pricedata,n,uppara,downpara,period){
  cci = CCI(HLC(pricedata),n=n)
  cci$precci = lag(cci,1)
  #向上穿越100 向下穿越-100 发出信号
  ccisignal = ifelse((cci$precci < uppara & cci$cci > uppara) | (cci$precci < downpara & cci$cci > downpara),'long',
                     ifelse((cci$precci > uppara & cci$cci < uppara) | (cci$precci > downpara & cci$cci < downpara),'short','hold'))
  return(ccisignal[period])
}

getRSIsignal = function(pricedata,n,up,down,period){
  rsi = RSI(Cl(pricedata),n=n)
  rsi$prersi = lag(rsi,1)
  #向上穿越70发出long信号，向下穿越30发出short信号，其余为hold
  rssignal = ifelse(rsi$prersi < up & rsi$EMA > up,'long',
                    ifelse(rsi$prersi > down & rsi$EMA < down,'short','hold'))
  return(rssignal[period])
}

getMACDsignal = function(pricedata,nfast,nslow,nsig,sep,period){
  macd = MACD(Cl(pricedata),nfast,nslow,nsig)
  macd$premacd = lag(macd$macd,1)
  #向上穿越0线，发出long，向下穿越0线，发出short，其余为hold
  signal = ifelse(macd$premacd < sep & macd$macd >sep,'long',ifelse(macd$premacd > sep & macd$macd< sep,'short','hold'))
  return(signal[period])
}

getADXsignal = function(pricedata,n,period)
{
  adx = ADX(HLC(pricedata),n=n)
  adx$predip = lag(adx$DIp,n=1)
  adx$predin = lag(adx$DIn,n=1)
  #ip向上穿越in发出long信号， ip向下穿越in 发出short信号，其余为hold
  signal = ifelse(adx$predip < adx$predin & adx$DIp > adx$DIn,'long',ifelse(adx$predin < adx$predip & adx$DIn > adx$DIp,'short','hold'))
  return(signal[period])
}

getMFIsignal = function(pricedata,n,up,down,period){
  mfi = MFI(HLC(pricedata),Vo(pricedata),n=n)
  mfi$premfi = lag(mfi,1)
  #向上穿越20发出弄信号 向下穿越80发出short信号 其余为hold信号
  signal = ifelse(mfi$premfi < up & mfi$mfi >= up,'long',ifelse(mfi$premfi > down & mfi$mfi < down,'short','hold'))
  return(signal[period])
}

getBBANDSsignal = function(pricedata,n,period){
  bbands = BBands(HLC(pricedata),n=n)
  bdata = merge(bbands,Cl(pricedata))
  bdata$precl = lag(bdata$Close,1)
  bdata$preup = lag(bdata$up,1)
  bdata$predn =lag(bdata$dn,1)
  #close价格向上穿越dn close价格向下穿越up 发出信号
  signal = ifelse(bdata$precl > bdata$predn & bdata$Close < bdata$dn,'long',
                  ifelse(bdata$precl < bdata$preup & bdata$Close > bdata$up,'short','hold'))
  return(signal[period])
}

getROCsignal = function(pricedata,n,sep,period){
  roc = ROC(Cl(pricedata),n=n)
  roc$pre = lag(roc,1)
  #穿越0线 发出信号
  signal = ifelse(roc$pre < sep & roc$Close >sep,'long',ifelse(roc$pre > sep & roc$Close < sep ,'short','hold'))
  return(signal[period])
}

getSARsignal = function(pricedata,ac1,ac2,period){
  sar = SAR(HLC(pricedata)[,c(1,2)],accel=c(ac1,ac2))
  sardata = merge(sar,Cl(pricedata))
  sardata$precl = lag(sardata$Close,1)
  sardata$presar = lag(sardata$sar,1)
  #sar下降，close上升，sar上升，close下降发出信号
  signal = ifelse(sardata$precl < sardata$presar & sardata$Close > sardata$sar,'long',
                  ifelse(sardata$precl > sardata$presar & sardata$Close < sardata$sar,'short','hold'))
  return(signal[period])
}

getWPRsignal = function(pricedata,n,up,down,period){
  wpr = WPR(HLC(pricedata),n=n)
  wpr$pre = lag(wpr$Close,1)
  #上穿0.8，下穿0.2
  signal = ifelse(wpr$pre < up & wpr$Close > up,'long',ifelse(wpr$pre > down & wpr$Close < down,'short','hold'))
  return(signal[period])
}

getKDJsignal = function(pricedata,nfk,nfd,nsd,period){
  kdj = stoch(HLC(pricedata), nFastK = nfk, nFastD = nfd, nSlowD = nsd)
  kdj$prefastk = lag(kdj$fastK,1)
  kdj$prefastd = lag(kdj$fastD,1)
  #k线穿越d线发出信号
  signal = ifelse( kdj$prefastk < kdj$prefastd & kdj$fastK > kdj$fastD,'long',
                   ifelse(kdj$prefastk > kdj$prefastd & kdj$fastK < kdj$fastD,'short','hold'))
  return(signal[period])
}

getTDIsignal = function(pricedata,n,sep,period){
  tdi = TDI(OHLC(pricedata),n=n)
  tdi$pretdi = lag(tdi$tdi,1)
  #上下穿越0线
  signal = ifelse(tdi$pretdi < sep & tdi$tdi > sep,'long',ifelse(tdi$pretdi > sep & tdi$tdi < sep,'short','hold'))
  return(signal[period])
}

getKSTsignal =  function(pricedata,n,period){
  kst = KST(Cl(pricedata))
  kst$sma = SMA(kst$kst,n=n)
  kst$prekst = lag(kst$kst,1)
  kst$presma = lag(kst$sma,1)
  #kst上下穿越均线
  signal = ifelse(kst$prekst < kst$presma & kst$kst > kst$sma,'long',
                  ifelse(kst$prekst > kst$presma & kst$kst < kst$sma,'short','hold'))
  return(signal[period])
}

getChaikinsignal = function(pricedata,period){
  chaikinad = chaikinAD(HLC(pricedata),Vo(pricedata))
  chaikdata = merge(Cl(pricedata),chaikinad)
  chaikdata$precl = lag(chaikdata$Close,1)
  chaikdata$prech = lag(chaikdata$chaikinad,1)
  #价格降，ad上升 价格升，ad下降
  signal = ifelse((chaikdata$Close - chaikdata$precl) < 0 & (chaikdata$chaikinad - chaikdata$prech ) > 0,'long',
                  ifelse((chaikdata$Close - chaikdata$precl) > 0 & (chaikdata$chaikinad - chaikdata$prech ) < 0,'short','hold'))
  return(signal[period])
}

getOBVsignal = function(pricedata,sep,period){
  obv = OBV(Cl(pricedata),Vo(pricedata))
  obvdata = merge(obv,OpCl(pricedata))
  names(obvdata) = c('obv','Close')
  obvdata$preobv = lag(obvdata$obv,1)
  #价格跌，obv上升，价格升，obv下降，发出信号
  signal = ifelse(obvdata$Close < sep & (obvdata$obv - obvdata$preobv) > sep ,'long',
                  ifelse(obvdata$Close > sep & (obvdata$obv - obvdata$preobv) < sep ,'short','hold'))
  return(signal[period])
}

getCMOsignal = function(pricedata,n,n1,period){
  cmo = CMO(Cl(pricedata),n=n)
  cmo = na.omit(cmo)
  names(cmo) = 'cmo'
  cmo$sma = SMA(cmo,n1)
  cmo$precmo = lag(cmo$cmo,1)
  cmo$presma = lag(cmo$sma,1)
  #上下穿越其均线，发出信号
  signal = ifelse(cmo$precmo < cmo$presma & cmo$cmo > cmo$sma,'long',ifelse(cmo$precmo > cmo$presma & cmo$cmo < cmo$sma,'short','hold'))
  return(signal[period])
}

getCMFsignal = function(pricedata,n,sep,period){
  cmf = CMF(HLC(pricedata),Vo(pricedata),n=n)
  names(cmf) = 'cmf'
  cmf$precmf = lag(cmf$cmf,1)
  #穿越0线，发出信号
  signal = ifelse( cmf$precmf < sep & cmf$cmf > sep,'long',ifelse(cmf$precmf > sep & cmf$cmf < sep,'short','hold'))
  return(signal[period])
}

getEMVsignal = function(pricedata,n,sep,period){
  emv = EMV(HLC(pricedata)[,c(1,2)],Vo(pricedata),n=n)
  emv$preemv = lag(emv$emv,1)
  #上下穿越0线
  signal = ifelse(emv$preemv < sep & emv$emv > sep,'long',ifelse(emv$preemv > sep & emv$emv < sep,'short','hold'))
  return(signal[period])
}

getTRIXsignal = function(pricedata,n,sig,period){
  trix = TRIX(Cl(pricedata),n = n, nSig = sig)
  trix$pretrix = lag(trix$TRIX,1)
  trix$presignal = lag(trix$signal,1)
  #上下穿越信号线
  signal = ifelse(trix$pretrix < trix$presignal & trix$TRIX > trix$signal,'long',
                  ifelse(trix$pretrix > trix$presignal & trix$TRIX < trix$signal,'short','hold'))
  return(signal[period])
}
getWilliamADsignal = function(pricedata,sep,period){
  williamsad = williamsAD(OHLC(pricedata)[,c(1,2,4)])
  willdata = merge(williamsad,OpCl(pricedata))
  names(willdata) = c('will','cl')
  willdata$prewill = lag(willdata$will,1)
  #close下降，will上升，close上升，will下降，发出信号
  signal = ifelse(willdata$cl < sep & (willdata$will - willdata$prewill) > sep,'long',
                  ifelse(willdata$cl > sep & (willdata$will - willdata$prewill) < sep,'short','hold'))
  return(signal[period])
}