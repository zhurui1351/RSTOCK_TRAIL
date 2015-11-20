getClosestatus = function(pricedata,n,period){
  sma =SMA(Cl(pricedata),n=n)
  close = Cl(pricedata)
  signal = ifelse( sma < close,'less','more')
  return(signal[period])
}

getSMAstatus = function(pricedata,short,long,period){
  smashort =SMA(Cl(pricedata),n=short)
  smalong =SMA(Cl(pricedata),n=long)
  #标识状态,短线在长线下方，标识为Less 否则为more
  signal = ifelse( smashort < smalong,'less','more')
  return(signal[period])
}

getCCIstatus = function(pricedata,n,up,down,period){
  cci = CCI(HLC(pricedata),n=n)
  ccisignal = ifelse( cci < down,'less',ifelse( cci > up,'more','middle'))
  return(ccisignal[period])
}

getRSIstatus = function(pricedata,n,up,down,period){
  rsi = RSI(Cl(pricedata),n=n)
  rssignal = ifelse(rsi$EMA < down,'less',ifelse(rsi$EMA > up,'more','middle'))
  return(rssignal[period])
}

getMACDstatus = function(pricedata,nfast,nslow,nsig,sep,period){
  macd = MACD(Cl(pricedata),nfast,nslow,nsig)
  signal = ifelse(macd$macd < sep,'less','more')
  return(signal[period])
}

getADXstatus = function(pricedata,n,period)
{
  adx = ADX(HLC(pricedata),n=n)
  signal = ifelse(adx$DIp > adx$DIn,'more','less')
  return(signal[period])
}

getMFIstatus = function(pricedata,n,up,down,period){
  mfi = MFI(HLC(pricedata),Vo(pricedata),n=n)
  signal = ifelse(mfi$mfi < down,'less',ifelse(mfi$mfi > up,'more','middle'))
  return(signal[period])
}

getBBANDSstatus = function(pricedata,n,period){
  bbands = BBands(HLC(pricedata),n=n)
  bdata = merge(bbands,Cl(pricedata))
  signal = ifelse(bdata$Close < bdata$dn,'less',
                  ifelse(bdata$Close > bdata$up,'more','middle'))
  return(signal[period])
}

getROCstatus = function(pricedata,n,sep,period){
  roc = ROC(Cl(pricedata),n=n)
  signal = ifelse(roc$Close >sep,'more',ifelse(roc$Close < sep ,'less','middle'))
  return(signal[period])
}

getSARstatus = function(pricedata,ac1,ac2,period){
  sar = SAR(HLC(pricedata)[,c(1,2)],accel=c(ac1,ac2))
  sardata = merge(sar,Cl(pricedata))
  signal = ifelse(sardata$Close > sardata$sar,'more',
                  ifelse(sardata$Close < sardata$sar,'less','middle'))
  return(signal[period])
}

getWPRstatus = function(pricedata,n,up,down,period){
  wpr = WPR(HLC(pricedata),n=n)
  signal = ifelse(wpr$Close > up,'more',ifelse(wpr$Close < down,'less','middle'))
  return(signal[period])
}

getKDJstatus = function(pricedata,nfk,nfd,nsd,period){
  kdj = stoch(HLC(pricedata), nFastK = nfk, nFastD = nfd, nSlowD = nsd)
  signal = ifelse( kdj$fastK > kdj$fastD,'more',
                   ifelse(kdj$fastK < kdj$fastD,'less','middle'))
  return(signal[period])
}

getTDIstatus = function(pricedata,n,sep,period){
  tdi = TDI(OHLC(pricedata),n=n)
  signal = ifelse(tdi$tdi > sep,'more','less')
  return(signal[period])
}

getKSTstatus =  function(pricedata,n,period){
  kst = KST(Cl(pricedata))
  kst = na.omit(kst)
  kst$sma = SMA(kst$kst,n=n)
  signal = ifelse(kst$kst > kst$sma,'more',
                  ifelse(kst$kst < kst$sma,'less','middle'))
  return(signal[period])
}
getChaikinVostatus = function(pricedata,n,period){
  chaikinad = chaikinVolatility(HLC(pricedata)[,c(1,2)])
  chaikinad = na.omit(chaikinad)
  chaikinad = chaikinad[chaikinad != Inf]
  chaikinad$sma = SMA(chaikinad$EMA,n=n)
  signal = ifelse(chaikinad$EMA > chaikinad$sma,'less','more')
  return(signal[period])
}

getOBVstatus = function(pricedata,n,period){
  obv = OBV(Cl(pricedata),Vo(pricedata))
  obv = na.omit(obv)
  obv$sma = SMA(obv,n=n)
  signal = ifelse(obv$obv > obv$sma ,'more','less')
  return(signal[period])
}

getCMOstatus = function(pricedata,n,n1,period){
  cmo = CMO(Cl(pricedata),n=n)
  cmo = na.omit(cmo)
  names(cmo) = 'cmo'
  cmo$sma = SMA(cmo,n1)
  signal = ifelse(cmo$cmo > cmo$sma,'more','less')
  return(signal[period])
}

getCMFstatus = function(pricedata,n,sep,period){
  cmf = CMF(HLC(pricedata),Vo(pricedata),n=n)
  names(cmf) = 'cmf'
  signal = ifelse( cmf$cmf > sep,'mroe','less')
  return(signal[period])
}

getEMVstatus = function(pricedata,n,sep,period){
  emv = EMV(HLC(pricedata)[,c(1,2)],Vo(pricedata),n=n)
  signal = ifelse(emv$emv > sep,'more','less')
  return(signal[period])
}

getTRIXstatus = function(pricedata,n,sig,period){
  trix = TRIX(Cl(pricedata),n = n, nSig = sig)
  signal = ifelse(trix$TRIX > trix$signal,'more','less')
  return(signal[period])
}
