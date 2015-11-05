#获取测试期间最优化的sma信号列表 shortpara = 3:10 longpara = 5:20
optimSMA = function(pricedata,analysedata,start,end,longpara,shortpara)
{
  getsignal = function(smalong,smashort){
    smashort$preshort = lag(smashort,1)
    smalong$prelong = lag(smalong,1)
    #打叉发出信号
    signal = ifelse( smashort$preshort < smalong$prelong & smashort$SMA >= smalong$SMA,'long',
                     ifelse(smashort$preshort > smalong$prelong & smashort$SMA <= smalong$SMA,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  result = data.frame()
  analysedata_train = analysedata[period]
  #遍历寻找最优解
  for( long in longpara)
  {
    for(short in shortpara)
    {
      if(short >= long)
      {
        next
      }
      smashort =SMA(Cl(pricedata),n=short)
      smalong =SMA(Cl(pricedata),n=long)
      smasignal = getsignal(smalong,smashort)
      smasignal = smasignal[period]
      colnames(smasignal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],smasignal)
      p[p=='hold'] = NA
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      a1 = tb$tables[[1]][1,1]
      a2 = tb$tables[[1]][2,1]
      #同时大于或小于0.5 则不考虑
      if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #找出距离最大的参数
      r = data.frame(a1=a1,a2=a2,short=short,long=long,abs=abs(a1 - a2))
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  
  s = subset(result,abs==max(result$abs))[1,]
  
  smashort =SMA(Cl(pricedata),n=s$short)
  smalong =SMA(Cl(pricedata),n=s$long)
  smasignal = getsignal(smalong,smashort)
  smasignal = smasignal[period]
  #可能需要衡量下不为hold的数量
  return(smasignal)
}

#最优化cci指标
#如uppara = seq(80,120,by=10) downpara= seq(-80 , -120,by=-10)  npara = 5 : 10
optimCCI = function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
  getsignal = function(cci,uppara,downpara){
    cci$precci = lag(cci,1)
    #向上穿越100 向下穿越-100 发出信号
    ccisignal = ifelse((cci$precci < uppara & cci$cci > uppara) | (cci$precci < downpara & cci$cci > downpara),'long',
                       ifelse((cci$precci > uppara & cci$cci < uppara) | (cci$precci > downpara & cci$cci < downpara),'short','hold'))
    return(ccisignal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(up in uppara)
    {
      for(down in downpara)
      {
      #  print(paste(n,up,down))
        cci = CCI(HLC(pricedata),n=n)
        ccisignal = getsignal(cci,up,down)
        ccisignal = ccisignal[period]
        colnames(ccisignal) = 'signal'
        p = merge(analysedata_train[,'leadclflag'],ccisignal)
        p[p=='hold'] = NA
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        
        a1 = tb$tables[[1]][1,1]
        a2 = tb$tables[[1]][2,1]
        #同时大于或小于0.5 则不考虑
        if(sign(a1-0.5) == sign(a2 - 0.5)) next
        #找出距离最大的参数
        r = data.frame(a1=a1,a2=a2,n=n,up=up,down=down,abs=abs(a1 - a2))
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  
  s = subset(result,abs==max(result$abs))[1,]
  cci = CCI(HLC(pricedata),n=s$n)
  ccisignal = getsignal(cci,s$up,s$down)
  ccisignal = ccisignal[period]
  return(ccisignal)
}

#如uppara = seq(60,90,by=10) downpara= seq(50 , 10,by=-10)  npara = 3 : 10 
optimRSI = function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
  getsignal = function(rsi,up,down){
    rsi$prersi = lag(rsi,1)
    #向上穿越70发出long信号，向下穿越30发出short信号，其余为hold
    rssignal = ifelse(rsi$prersi < up & rsi$EMA > up,'long',
                      ifelse(rsi$prersi > down & rsi$EMA < down,'short','hold'))
    return(rssignal)
  }
  
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  
  for(n in npara)
  {
    for(up in uppara)
    {
      for(down in downpara)
      {
        rsi = RSI(Cl(pricedata),n=n)
        rsisignal = getsignal(rsi,up,down)
        rsisignal = rsisignal[period]
        colnames(rsisignal) ='signal'
        
        p = merge(analysedata_train[,'leadclflag'],rsisignal)
        p[p=='hold'] = NA
        
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        
        a1 = tb$tables[[1]][1,1]
        a2 = tb$tables[[1]][2,1]
        #同时大于或小于0.5 则不考虑
        if(sign(a1-0.5) == sign(a2 - 0.5)) next
        #找出距离最大的参数
        r = data.frame(a1=a1,a2=a2,n=n,up=up,down=down,abs=abs(a1 - a2))
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  
  s = subset(result,abs==max(result$abs))[1,]
  rsi = RSI(Cl(pricedata),n=s$n)
  rsisignal = getsignal(rsi,s$up,s$down)
  rsisignal = rsisignal[period]
  return(ccisignal)
}

#最优化macd nfastpara =seq(3,16,by=2)  nslowpara = seq(15,30,by=2) nsigpara=seq(5,12,by=2) seppara=seq(-20,20,5)
optimMACD = function(pricedata,analysedata,start,end,nfastpara,nslowpara,nsigpara,seppara)
{
  getsignal = function(macd,sep){
    macd$premacd = lag(macd$macd,1)
    #向上穿越0线，发出long，向下穿越0线，发出short，其余为hold
    signal = ifelse(macd$premacd < sep & macd$macd >sep,'long',ifelse(macd$premacd > sep & macd$macd< sep,'short','hold'))
    return(signal)
  }
  
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(nfast in nfastpara)
  {
    for(nslow in nslowpara)
    {
      for(nsig in nsigpara)
      {
        for( sep in seppara)
        {
          if(nfast > nslow) next
          if(nsig <= nfast || nsig >= nslow) next
          macd = MACD(Cl(pricedata),nfast,nslow,nsig)
          macdsignal = getsignal(macd,sep)
          macdsignal = macdsignal[period]
          colnames(macdsignal) ='signal'
          
          p = merge(analysedata_train[,'leadclflag'],macdsignal)
          p[p=='hold'] = NA
          
          #有效信号占比小于10% 过滤
          effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
          if(effecratio < 0.1) next
          
          tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
          
          a1 = tb$tables[[1]][1,1]
          a2 = tb$tables[[1]][2,1]
          #同时大于或小于0.5 则不考虑
          if(sign(a1-0.5) == sign(a2 - 0.5)) next
          #找出距离最大的参数
          r = data.frame(a1=a1,a2=a2,nslow=nslow,nfast=nfast,nsig=nsig,sep=sep,abs=abs(a1 - a2))
          result = rbind(result,r)
        }
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  macd = MACD(Cl(pricedata),nFast=s$nfast,nSlow=s$nslow,nSig=s$nsig)
  macdsignal = getsignal(macd,s$sep)
  macdsignal = macdsignal[period]
  return(macdsignal)
}
#npara = seq(3,20,by=2)
 optimADX = function(pricedata,analysedata,start,end,npara)
 {
   getsignal = function(adx)
   {
     adx$predip = lag(adx$DIp,n=1)
     adx$predin = lag(adx$DIn,n=1)
     #ip向上穿越in发出long信号， ip向下穿越in 发出short信号，其余为hold
     signal = ifelse(adx$predip < adx$predin & adx$DIp > adx$DIn,'long',ifelse(adx$predin < adx$predip & adx$DIn > adx$DIp,'short','hold'))
     return(signal)
   }
   period = paste(start,end,sep='/')
   analysedata_train = analysedata[period]
   result = data.frame()
   for(n in npara)
   {
     adx = ADX(HLC(pricedata),n=n)
     adxsignal = getsignal(adx)
     adxsignal = adxsignal[period]
     colnames(adxsignal) ='signal'
     
     p = merge(analysedata_train[,'leadclflag'],adxsignal)
     p[p=='hold'] = NA
     
     #有效信号占比小于10% 过滤
     effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
     if(effecratio < 0.1) next
     
     tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
     
     a1 = tb$tables[[1]][1,1]
     a2 = tb$tables[[1]][2,1]
     #同时大于或小于0.5 则不考虑
     if(sign(a1-0.5) == sign(a2 - 0.5)) next
     #找出距离最大的参数
     r = data.frame(a1=a1,a2=a2,n=n,abs=abs(a1 - a2))
     result = rbind(result,r)
   }
   if(nrow(result) == 0) return(NA)
   s = subset(result,abs==max(result$abs))[1,]
   adx = ADX(HLC(pricedata),n=s$n)
   adxsignal = adxsignal[period]
   return(adxsignal)
 }
 
 # npara = seq(3,20,by=2) uppara = seq(30,0,by=-10) downpara = seq(60,100,by=10)
optimMFI =function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
  getsignal = function(mfi,up,down){
    mfi$premfi = lag(mfi,1)
    #向上穿越20发出弄信号 向下穿越80发出short信号 其余为hold信号
    signal = ifelse(mfi$premfi < up & mfi$mfi >= up,'long',ifelse(mfi$premfi > down & mfi$mfi < down,'short','hold'))
    return(signal)
  }
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(up in uppara)
    {
      for(down in downpara)
      {
        mfi = MFI(HLC(pricedata),Vo(pricedata),n=n)
        mfisignal = getsignal(mfi,up,down)
        mfisignal = mfisignal[period]
        colnames(mfisignal) = 'signal'
        
        p = merge(analysedata_train[,'leadclflag'],mfisignal)
        p[p=='hold'] = NA
        
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        
        a1 = tb$tables[[1]][1,1]
        a2 = tb$tables[[1]][2,1]
        #同时大于或小于0.5 则不考虑
        if(sign(a1-0.5) == sign(a2 - 0.5)) next
        #找出距离最大的参数
        r = data.frame(a1=a1,a2=a2,n=n,up=up,down=down,abs=abs(a1 - a2))
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  mfi = MFI(HLC(pricedata),Vo(pricedata),n=s$n)
  mfisignal = getsignal(mfi,s$up,s$down)
  mfisignal = mfisignal[period]
  return(mfisignal)
}

# n = 3:20
optimBBANDS = function(pricedata,analysedata,start,end,npara)
{
  getsignal = function(bbands,Close){
    bdata = merge(bbands,Close)
    bdata$precl = lag(bdata$Close,1)
    bdata$preup = lag(bdata$up,1)
    bdata$predn =lag(bdata$dn,1)
    #close价格向上穿越dn close价格向下穿越up 发出信号
    signal = ifelse(bdata$precl > bdata$predn & bdata$Close < bdata$dn,'long',
                    ifelse(bdata$precl < bdata$preup & bdata$Close > bdata$up,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    bbands = BBands(HLC(pricedata),n=n)
    bbandssignal = getsignal(bbands,Cl(pricedata))
    bbandssignal = bbandssignal[period]
    
    colnames(bbandssignal) = 'signal'
    
    p = merge(analysedata_train[,'leadclflag'],bbandssignal)
    p[p=='hold'] = NA
    
    #有效信号占比小于10% 过滤
    effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
    if(effecratio < 0.1) next
    
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    
    a1 = tb$tables[[1]][1,1]
    a2 = tb$tables[[1]][2,1]
    #同时大于或小于0.5 则不考虑
    if(sign(a1-0.5) == sign(a2 - 0.5)) next
    #找出距离最大的参数
    r = data.frame(a1=a1,a2=a2,n=n,abs=abs(a1 - a2))
    result = rbind(result,r)
    
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  bbands = BBands(HLC(pricedata),n=s$n)
  bbandssignal = getsignal(bbands,Cl(pricedata))
  bbandssignal = bbandssignal[period]
  return(bbandssignal)
}

#npara = seq(3,21,2) seppara = seq(-20,20,10)
optimROC = function(pricedata,analysedata,start,end,npara,seppara)
{
  getsignal = function(roc,sep){
    roc$pre = lag(roc,1)
    #穿越0线 发出信号
    signal = ifelse(roc$pre < sep & roc$Close >sep,'long',ifelse(roc$pre > sep & roc$Close < sep ,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  
  for(n in npara)
  {
    for(sep in seppara)
    {
      roc = ROC(Cl(pricedata),n=n)
      rocsignal = getsignal(roc,sep)
      rocsignal = rocsignal[period]
      colnames(rocsignal) ='signal'
      
      p = merge(analysedata_train[,'leadclflag'],rocsignal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
      a1 = tb$tables[[1]][1,1]
      a2 = tb$tables[[1]][2,1]
      #同时大于或小于0.5 则不考虑
      if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #找出距离最大的参数
      r = data.frame(a1=a1,a2=a2,n=n,sep=sep,abs=abs(a1 - a2))
      result = rbind(result,r)
    }
  }
  
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  roc = ROC(Cl(pricedata),n=s$n)
  rocsignal = getsignal(roc,s$sep)
  rocsignal = rocsignal[period]
  return(rocsignal)
}

#a1para = seq(0.01,0.09,0.01) a2para = seq(0.1,0.5,0.1)
optimSAR = function(pricedata,analysedata,start,end,a1para,a2para)
{
  getsignal = function(sar,Close){
    sardata = merge(sar,Close)
    sardata$precl = lag(sardata$Close,1)
    sardata$presar = lag(sardata$sar,1)
    #sar下降，close上升，sar上升，close下降发出信号
    signal = ifelse(sardata$precl < sardata$presar & sardata$Close > sardata$sar,'long',
                    ifelse(sardata$precl > sardata$presar & sardata$Close < sardata$sar,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(ac1 in a1para)
  {
    for(ac2 in a2para)
    {
      sar = SAR(HLC(pricedata)[,c(1,2)],accel=c(ac1,ac2))
      sarsignal = getsignal(sar,Cl(pricedata))
      sarsignal = sarsignal[period]
      colnames(sarsignal) ='signal'
      p = merge(analysedata_train[,'leadclflag'],sarsignal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
      a1 = tb$tables[[1]][1,1]
      a2 = tb$tables[[1]][2,1]
      #同时大于或小于0.5 则不考虑
      if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #找出距离最大的参数
      r = data.frame(a1=a1,a2=a2,ac1=ac1,ac2=ac2,abs=abs(a1 - a2))
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  sar = SAR(HLC(pricedata)[,c(1,2)],accel=c(s$ac1,s$ac2))
  sarsignal = getsignal(sar,Cl(pricedata))
  sarsignal = sarsignal[period]
  return(sarsignal)
}

#npara = seq(3,15,2) uppara = seq(0.6,0.9,0.1) downpara = seq(0.4,0.1,-0.1)
optimWPR = function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
  getsignal = function(wpr,up,down){
    wpr$pre = lag(wpr$Close,1)
    #上穿0.8，下穿0.2
    signal = ifelse(wpr$pre < up & wpr$Close > up,'long',ifelse(wpr$pre > down & wpr$Close < down,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(up in uppara)
    {
      for(down in downpara)
      {
        wpr = WPR(HLC(pricedata),n=n)
        wprsignal = getsignal(wpr,up,down)
        wprsignal = wprsignal[period]
        colnames(wprsignal) ='signal'
        p = merge(analysedata_train[,'leadclflag'],wprsignal)
        p[p=='hold'] = NA
        
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        
        a1 = tb$tables[[1]][1,1]
        a2 = tb$tables[[1]][2,1]
        #同时大于或小于0.5 则不考虑
        if(sign(a1-0.5) == sign(a2 - 0.5)) next
        #找出距离最大的参数
        r = data.frame(a1=a1,a2=a2,n=n,up=up,down=down,abs=abs(a1 - a2))
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  wpr = WPR(HLC(pricedata),n=s$n)
  wprsignal = getsignal(wpr,s$up,s$down)
  wprsignal = wprsignal[period]
}

#nfkpara = seq(5,20,2) nfdpara=seq(3,11,2) nsdpara = seq(3,11,2)
# 类似smi指标
optimKDJ = function(pricedata,analysedata,start,end,nfkpara,nfdpara,nsdpara)
{
  getsignal = function(x){
    kdj$prefastk = lag(kdj$fastK,1)
    kdj$prefastd = lag(kdj$fastD,1)
    #k线穿越d线发出信号
    signal = ifelse( kdj$prefastk < kdj$prefastd & kdj$fastK > kdj$fastD,'long',
                     ifelse(kdj$prefastk > kdj$prefastd & kdj$fastK < kdj$fastD,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(nfk in nfkpara)
  {
    for(nfd in nfdpara)
    {
      for(nsd in nsdpara)
      {
        kdj = stoch(HLC(pricedata), nFastK = nfk, nFastD = nfd, nSlowD = nsd)
        kdjsignal = getsignal(kdj)
        kdjsignal = kdjsignal[period]
        colnames(kdjsignal) = 'signal'
        
        p = merge(analysedata_train[,'leadclflag'],kdjsignal)
        p[p=='hold'] = NA
        
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        
        a1 = tb$tables[[1]][1,1]
        a2 = tb$tables[[1]][2,1]
        #同时大于或小于0.5 则不考虑
        if(sign(a1-0.5) == sign(a2 - 0.5)) next
        #找出距离最大的参数
        r = data.frame(a1=a1,a2=a2,nfk=nfk,nfd=nfd,nsd=nsd,abs=abs(a1 - a2))
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  kdj = stoch(HLC(pricedata), nFastK = s$nfk, nFastD = s$nfd, nSlowD = s$nsd)
  kdjsignal = getsignal(kdj)
  kdjsignal = kdjsignal[period]
  return(kdjsignal)  
}

#smi 同 kdj类似

# npara = seq(3,20,2) seppara =seq(-10,10,5)
optimTDI = function(pricedata,analysedata,start,end,npara,seppara)
{
  getsignal = function(tdi,sep){
    tdi$pretdi = lag(tdi$tdi,1)
    #上下穿越0线
    signal = ifelse(tdi$pretdi < sep & tdi$tdi > sep,'long',ifelse(tdi$pretdi > sep & tdi$tdi < sep,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sep in seppara)
    {
      tdi = TDI(OHLC(pricedata),n=n)
      tdisignal = getsignal(tdi,sep)
      tdisignal = tdisignal[period]
      colnames(tdisignal) = 'signal'
      
      p = merge(analysedata_train[,'leadclflag'],tdisignal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
      a1 = tb$tables[[1]][1,1]
      a2 = tb$tables[[1]][2,1]
      #同时大于或小于0.5 则不考虑
      if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #找出距离最大的参数
      r = data.frame(a1=a1,a2=a2,n=n,sep=sep,abs=abs(a1 - a2))
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  tdi = TDI(OHLC(pricedata),n=s$n)
  tdisignal = getsignal(tdi,s$sep)
  tdisignal = tdisignal[period]
  return(tdisignal)
}

#npara = seq(3,20,2)
optimKST = function(pricedata,analysedata,start,end,npara)
{
  getsignal =  function(kst,n){
    kst$sma = SMA(kst$kst,n=n)
    kst$prekst = lag(kst$kst,1)
    kst$presma = lag(kst$sma,1)
    #kst上下穿越均线
    signal = ifelse(kst$prekst < kst$presma & kst$kst > kst$sma,'long',
                    ifelse(kst$prekst > kst$presma & kst$kst < kst$sma,'short','hold'))
    return(signal)
  }
  
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    kst = KST(Cl(pricedata))
    kstsignal = getsignal(kst,n)
    kstsignal = kstsignal[period]
    colnames(kstsignal) = 'signal'
    
    p = merge(analysedata_train[,'leadclflag'],kstsignal)
    p[p=='hold'] = NA
    
    #有效信号占比小于10% 过滤
    effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
    if(effecratio < 0.1) next
    
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    
    a1 = tb$tables[[1]][1,1]
    a2 = tb$tables[[1]][2,1]
    #同时大于或小于0.5 则不考虑
    if(sign(a1-0.5) == sign(a2 - 0.5)) next
    #找出距离最大的参数
    r = data.frame(a1=a1,a2=a2,n=n,abs=abs(a1 - a2))
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  kst = KST(Cl(pricedata))
  kstsignal = getsignal(kst,s$n)
  kstsignal = kstsignal[period]
  return(kstsignal)
}

#无参数优化
optimChaikinAD = function(pricedata,analysedata,start,end)
{
  getsignal = function(chaikinad,close){
    chaikdata = merge(close,chaikinad)
    chaikdata$precl = lag(chaikdata$Close,1)
    chaikdata$prech = lag(chaikdata$chaikinad,1)
    #价格降，ad上升 价格升，ad下降
    signal = ifelse((chaikdata$Close - chaikdata$precl) < 0 & (chaikdata$chaikinad - chaikdata$prech ) > 0,'long',
                    ifelse((chaikdata$Close - chaikdata$precl) > 0 & (chaikdata$chaikinad - chaikdata$prech ) < 0,'short','hold'))
    return(signal)
  }
  chaikinad = chaikinAD(HLC(pricedata),Vo(pricedata))
  chaikinadsignal = getsignal(chaikinad,Cl(pricedata))
  period = paste(start,end,sep='/')
  chaikinadsignal = chaikinadsignal[period]
  effecratio = length(chaikinadsignal[chaikinadsignal!='hold']) / length(chaikinadsignal)
  if(effecratio < 0.1) return(NA)
  else return(chaikinadsignal)
}

# seppara = seq(10,-10,-5)
optimOBV = function(pricedata,analysedata,start,end,seppara)
{
  getsignal = function(obv,close,sep){
    obvdata = merge(obv,close)
    names(obvdata) = c('obv','Close')
    obvdata$preobv = lag(obvdata$obv,1)
    #价格跌，obv上升，价格升，obv下降，发出信号
    signal = ifelse(obvdata$Close < sep & (obvdata$obv - obvdata$preobv) > sep ,'long',
                    ifelse(obvdata$Close > sep & (obvdata$obv - obvdata$preobv) < sep ,'short','hold'))
    return(signal)
  }
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(sep in seppara)
  {
    obv = OBV(Cl(pricedata),Vo(pricedata))
    obvsignal = getsignal(obv,OpCl(pricedata),sep)
    obvsignal = obvsignal[period]
    colnames(obvsignal) = 'signal'
    
    p = merge(analysedata_train[,'leadclflag'],obvsignal)
    p[p=='hold'] = NA
    
    #有效信号占比小于10% 过滤
    effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
    if(effecratio < 0.01) next
    
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    
    a1 = tb$tables[[1]][1,1]
    a2 = tb$tables[[1]][2,1]
    #同时大于或小于0.5 则不考虑
    if(sign(a1-0.5) == sign(a2 - 0.5)) next
    #找出距离最大的参数
    r = data.frame(a1=a1,a2=a2,sep=sep,abs=abs(a1 - a2))
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  obv = OBV(Cl(pricedata),Vo(pricedata))
  obvsignal = getsignal(obv,OpCl(pricedata),s$sep)
  obvsignal = obvsignal[period]
  return(obvsignal)
}

#npara = seq(3,20,2) n1para = seq(3,20,2)
optimCMO = function(pricedata,analysedata,start,end,npara,n1para)
{
  getsignal = function(cmo,n1){
    names(cmo) = 'cmo'
    cmo$sma = SMA(cmo,n1)
    cmo$precmo = lag(cmo$cmo,1)
    cmo$presma = lag(cmo$sma,1)
    #上下穿越其均线，发出信号
    signal = ifelse(cmo$precmo < cmo$presma & cmo$cmo > cmo$sma,'long',ifelse(cmo$precmo > cmo$presma & cmo$cmo < cmo$sma,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(n1 in n1para)
    {
      cmo = CMO(Cl(pricedata),n=n)
      cmosignal = getsignal(cmo,n1)
      cmosignal = cmosignal[period]
      colnames(cmosignal) = 'signal'
      
      p = merge(analysedata_train[,'leadclflag'],cmosignal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.01) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
      a1 = tb$tables[[1]][1,1]
      a2 = tb$tables[[1]][2,1]
      #同时大于或小于0.5 则不考虑
      if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #找出距离最大的参数
      r = data.frame(a1=a1,a2=a2,n=n,n1=n1,abs=abs(a1 - a2))
      result = rbind(result,r)
      
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  cmo = CMO(Cl(pricedata),n=s$n)
  cmosignal = getsignal(cmo,s$n1)
  cmosignal = cmosignal[period]
  return(cmosignal)
}

#npara = seq(3,21,2) seppara = seq(10,-10,-5)
optimCMF = function(pricedata,analysedata,start,end,npara,seppara)
{
  getsignal = function(cmf,sep){
    names(cmf) = 'cmf'
    cmf$precmf = lag(cmf$cmf,1)
    #穿越0线，发出信号
    signal = ifelse( cmf$precmf < sep & cmf$cmf > sep,'long',ifelse(cmf$precmf > sep & cmf$cmf < sep,'short','hold'))
    return(signal)
  }
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sep in seppara)
    {
      cmf = CMF(HLC(pricedata),Vo(pricedata),n=n)
      cmfsignal = getsignal(cmf,sep)
      cmfsignal = cmfsignal[period]
      colnames(cmfsignal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],cmfsignal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.01) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
      a1 = tb$tables[[1]][1,1]
      a2 = tb$tables[[1]][2,1]
      #同时大于或小于0.5 则不考虑
      if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #找出距离最大的参数
      r = data.frame(a1=a1,a2=a2,n=n,sep=sep,abs=abs(a1 - a2))
      result = rbind(result,r)
      
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  cmf = CMF(HLC(pricedata),Vo(pricedata),n=s$n)
  cmfsignal = getsignal(cmf,s$sep)
  cmfsignal = cmfsignal[period]
}

# npara = seq(3,21,2) seppara = seq(10,-10,-5)
optimEMV = function(pricedata,analysedata,start,end,npara,seppara)
{
  getsignal = function(emv,sep){
    emv$preemv = lag(emv$emv,1)
    #上下穿越0线
    signal = ifelse(emv$preemv < sep & emv$emv > sep,'long',ifelse(emv$preemv > sep & emv$emv < sep,'short','hold'))
    return(signal)
  }
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sep in seppara)
    {
      emv = EMV(HLC(pricedata)[,c(1,2)],Vo(pricedata),n=n)
      emvsignal = getsignal(emv,sep) 
      emvsignal = emvsignal[period]
      colnames(emvsignal) = 'signal'
      
      p = merge(analysedata_train[,'leadclflag'],emvsignal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.01) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
      a1 = tb$tables[[1]][1,1]
      a2 = tb$tables[[1]][2,1]
      #同时大于或小于0.5 则不考虑
      if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #找出距离最大的参数
      r = data.frame(a1=a1,a2=a2,n=n,sep=sep,abs=abs(a1 - a2))
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  emv = EMV(HLC(pricedata)[,c(1,2)],Vo(pricedata),n=s$n)
  emvsignal = getsignal(emv,s$sep) 
  emvsignal = emvsignal[period]
   
}

# npara = seq(3,21,2) sigpara = seq(3,10,2)
optimTRIX = function(pricedata,analysedata,start,end,npara,sigpara)
{
  getsignal = function(trix){
    trix$pretrix = lag(trix$TRIX,1)
    trix$presignal = lag(trix$signal,1)
    #上下穿越信号线
    signal = ifelse(trix$pretrix < trix$presignal & trix$TRIX > trix$signal,'long',
                    ifelse(trix$pretrix > trix$presignal & trix$TRIX < trix$signal,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sig in sigpara)
    {
      trix = TRIX(Cl(pricedata),n = n, nSig = sig)
      trixsignal = getsignal(trix)
      trixsignal = trixsignal[period]
      colnames(trixsignal) = 'signal'
      
      p = merge(analysedata_train[,'leadclflag'],trixsignal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.01) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
      a1 = tb$tables[[1]][1,1]
      a2 = tb$tables[[1]][2,1]
      #同时大于或小于0.5 则不考虑
      if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #找出距离最大的参数
      r = data.frame(a1=a1,a2=a2,n=n,sig=sig,abs=abs(a1 - a2))
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  trix = TRIX(Cl(pricedata),n = s$n, nSig = s$sig)
  trixsignal = getsignal(trix)
  trixsignal = trixsignal[period]
}

# seppara = seq(10,-10,-5)
optimWilliamsAD = function(pricedata,analysedata,start,end,seppara)
{
  getsignal = function(williamsad,close,sep){
    willdata = merge(williamsad,close)
    names(willdata) = c('will','cl')
    willdata$prewill = lag(willdata$will,1)
    #close下降，will上升，close上升，will下降，发出信号
    signal = ifelse(willdata$cl < sep & (willdata$will - willdata$prewill) > sep,'long',
                    ifelse(willdata$cl > sep & (willdata$will - willdata$prewill) < sep,'short','hold'))
    return(signal)
  }
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  
  for(sep in seppara)
  {
    williamsad = williamsAD(OHLC(pricedata)[,c(1,2,4)])
    williamsadsignal = getsignal(williamsad,OpCl(pricedata),sep)
    williamsadsignal = williamsadsignal[period]
    colnames(williamsadsignal) = 'signal'
    
    p = merge(analysedata_train[,'leadclflag'],williamsadsignal)
    p[p=='hold'] = NA
    
    #有效信号占比小于10% 过滤
    effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
    if(effecratio < 0.01) next
    
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    
    a1 = tb$tables[[1]][1,1]
    a2 = tb$tables[[1]][2,1]
    #同时大于或小于0.5 则不考虑
    if(sign(a1-0.5) == sign(a2 - 0.5)) next
    #找出距离最大的参数
    r = data.frame(a1=a1,a2=a2,n=n,sig=sig,abs=abs(a1 - a2))
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,abs==max(result$abs))[1,]
  williamsad = williamsAD(OHLC(pricedata)[,c(1,2,4)])
  williamsadsignal = getsignal(williamsad,OpCl(pricedata),s$sep)
  williamsadsignal = williamsadsignal[period]
}