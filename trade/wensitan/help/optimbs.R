#获取测试期间最优化的sma信号列表
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
      smashort = smashort[period]
      smalong = smalong[period]
      smasignal = getsignal(smalong,smashort)
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
  s = subset(result,abs==max(result$abs))[1,]
  
  smashort =SMA(Cl(pricedata),n=s$short)
  smalong =SMA(Cl(pricedata),n=s$long)
  smashort = smashort[period]
  smalong = smalong[period]
  smasignal = getsignal(smalong,smashort)
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
        cci = cci[period]
        ccisignal = getsignal(cci,up,down)
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
  s = subset(result,abs==max(result$abs))[1,]
  cci = CCI(HLC(pricedata),n=s$n)
  cci = cci[period]
  ccisignal = getsignal(cci,s$up,s$down)
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
        rsi = rsi[period]
        rsisignal = getsignal(rsi,up,down)
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
  s = subset(result,abs==max(result$abs))[1,]
  rsi = RSI(Cl(pricedata),n=s$n)
  rsi = rsi[period]
  rsisignal = getsignal(rsi,s$up,s$down)
  return(ccisignal)
}


optimMACD = function()
{
  
}


