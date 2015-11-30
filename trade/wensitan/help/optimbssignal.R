#获取测试期间最优化的sma信号列表 shortpara = 3:10 longpara = 5:20
optimSMA = function(pricedata,analysedata,start,end,longpara,shortpara)
{
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
     
      signal = getSMAsignal(pricedata,short,long,period)
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      p[p=='hold'] = NA
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
   #   a1 = tb$tables[[1]][1,1]
    #  a2 = tb$tables[[1]][2,1]
      #同时大于或小于0.5 则不考虑
    #  if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #if(a1 > 0.5 || a2 < 0.5) next
     # #if(abs(a1-a2) < 0.03) next
      #找出距离最大的参数
      
      precise =  fitness(tb,na.omit(as.data.frame(p)))
      if(precise < 0.5) next
      
      r = data.frame(short=short,long=long,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  
  s = subset(result,precise==max(result$precise))[1,]
  
  #可能需要衡量下不为hold的数量
  return(s)
}

#最优化cci指标
#如uppara = seq(80,120,by=10) downpara= seq(-80 , -120,by=-10)  npara = 5 : 10
optimCCI = function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
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
        signal = getCCIsignal(pricedata,n,up,down,period)
        colnames(signal) = 'signal'
        p = merge(analysedata_train[,'leadclflag'],signal)
        p[p=='hold'] = NA
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
         tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
#         
#         a1 = tb$tables[[1]][1,1]
#         a2 = tb$tables[[1]][2,1]
#         #同时大于或小于0.5 则不考虑
#         if(sign(a1-0.5) == sign(a2 - 0.5)) next
        #if(a1 > 0.5 || a2 < 0.5) next
        ##if(abs(a1-a2) < 0.03) next
        #找出距离最大的参数
      #  r = data.frame(n=n,up=up,down=down,abs=abs(a1 - a2))
        precise =  fitness(tb,na.omit(as.data.frame(p)))
        if(precise < 0.5) next
        
        r = data.frame(n=n,up=up,down=down,precise=precise)
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#如uppara = seq(60,90,by=10) downpara= seq(50 , 10,by=-10)  npara = 3 : 10 
optimRSI = function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  
  for(n in npara)
  {
    for(up in uppara)
    {
      for(down in downpara)
      {
        signal = getRSIsignal(pricedata,n,up,down,period)
        colnames(signal) ='signal'
        
        p = merge(analysedata_train[,'leadclflag'],signal)
        p[p=='hold'] = NA
        
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        
#         a1 = tb$tables[[1]][1,1]
#         a2 = tb$tables[[1]][2,1]
#         #同时大于或小于0.5 则不考虑
#         if(sign(a1-0.5) == sign(a2 - 0.5)) next
        # if(a1 > 0.5 || a2 < 0.5) next
        ##if(abs(a1-a2) < 0.03) next
        #找出距离最大的参数
        precise =  fitness(tb,na.omit(as.data.frame(p)))
        if(precise < 0.5) next
        
        r = data.frame(n=n,up=up,down=down,precise=precise)
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#最优化macd nfastpara =seq(3,16,by=2)  nslowpara = seq(15,30,by=2) nsigpara=seq(5,12,by=2) seppara=seq(-20,20,5)
optimMACD = function(pricedata,analysedata,start,end,nfastpara,nslowpara,nsigpara,seppara)
{
  
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

          signal = getMACDsignal(pricedata,nfast,nslow,nsig,sep,period)
          colnames(signal) ='signal'
          
          p = merge(analysedata_train[,'leadclflag'],signal)
          p[p=='hold'] = NA
          
          #有效信号占比小于10% 过滤
          effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
          if(effecratio < 0.1) next
          
          tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
          
#           a1 = tb$tables[[1]][1,1]
#           a2 = tb$tables[[1]][2,1]
#           #同时大于或小于0.5 则不考虑
#           if(sign(a1-0.5) == sign(a2 - 0.5)) next
          #if(a1 > 0.5 || a2 < 0.5) next
          ##if(abs(a1-a2) < 0.03) next
          #找出距离最大的参数
          precise = fitness(tb,na.omit(as.data.frame(p)))
          if(precise < 0.5) next
          
          r = data.frame(nslow=nslow,nfast=nfast,nsig=nsig,sep=sep,precise=precise)
          result = rbind(result,r)
        }
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}
#npara = seq(3,20,by=2)
 optimADX = function(pricedata,analysedata,start,end,npara)
 {
   period = paste(start,end,sep='/')
   analysedata_train = analysedata[period]
   result = data.frame()
   for(n in npara)
   {
     signal = getADXsignal(pricedata,n,period)
     colnames(signal) ='signal'
     
     p = merge(analysedata_train[,'leadclflag'],signal)
     p[p=='hold'] = NA
     
     #有效信号占比小于10% 过滤
     effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
     if(effecratio < 0.1) next
     tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
#      a1 = tb$tables[[1]][1,1]
#      a2 = tb$tables[[1]][2,1]
#      #同时大于或小于0.5 则不考虑
#      if(sign(a1-0.5) == sign(a2 - 0.5)) next
     #if(a1 > 0.5 || a2 < 0.5) next
     ##if(abs(a1-a2) < 0.03) next
     #找出距离最大的参数
     precise =  fitness(tb,na.omit(as.data.frame(p)))
     if(precise < 0.5) next
     
     r = data.frame(n=n,precise=precise)
     result = rbind(result,r)
   }
   if(nrow(result) == 0) return(NA)
   s = subset(result,precise==max(result$precise))[1,]
   return(s)
 }
 
 # npara = seq(3,20,by=2) uppara = seq(30,0,by=-10) downpara = seq(60,100,by=10)
optimMFI =function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
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
        signal = getMFIsignal(pricedata,n,up,down,period)
        colnames(signal) = 'signal'
        
        p = merge(analysedata_train[,'leadclflag'],signal)
        p[p=='hold'] = NA
        
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        
        
#         a1 = tb$tables[[1]][1,1]
#         a2 = tb$tables[[1]][2,1]
#         #同时大于或小于0.5 则不考虑
#         if(sign(a1-0.5) == sign(a2 - 0.5)) next
      # if(a1 > 0.5 || a2 < 0.5) next
      ##if(abs(a1-a2) < 0.03) next
        #找出距离最大的参数
        
        precise =  fitness(tb,na.omit(as.data.frame(p)))
        if(precise < 0.5) next
        
        r = data.frame(n=n,up=up,down=down,precise=precise)
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

# n = 3:20
optimBBANDS = function(pricedata,analysedata,start,end,npara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    
    signal = getBBANDSsignal(pricedata,n,period)
    colnames(signal) = 'signal'
    
    p = merge(analysedata_train[,'leadclflag'],signal)
    p[p=='hold'] = NA
    
    #有效信号占比小于10% 过滤
    effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
    if(effecratio < 0.1) next
    
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
#     
#     a1 = tb$tables[[1]][1,1]
#     a2 = tb$tables[[1]][2,1]
#     #同时大于或小于0.5 则不考虑
#     if(sign(a1-0.5) == sign(a2 - 0.5)) next
    #if(a1 > 0.5 || a2 < 0.5) next
   ##if(abs(a1-a2) < 0.03) next
    #找出距离最大的参数
    precise =  fitness(tb,na.omit(as.data.frame(p)))
    if(precise < 0.5) next
    
    r = data.frame(n=n,precise=precise)
    result = rbind(result,r)
    
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#npara = seq(3,21,2) seppara = seq(-20,20,10)
optimROC = function(pricedata,analysedata,start,end,npara,seppara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  
  for(n in npara)
  {
    for(sep in seppara)
    {
      signal = getROCsignal(pricedata,n,sep,period)
      colnames(signal) ='signal'
      
      p = merge(analysedata_train[,'leadclflag'],signal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
#       
#       a1 = tb$tables[[1]][1,1]
#       a2 = tb$tables[[1]][2,1]
#       #同时大于或小于0.5 则不考虑
#       if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #if(a1 > 0.5 || a2 < 0.5) next
     ##if(abs(a1-a2) < 0.03) next
      #找出距离最大的参数
      precise =  fitness(tb,na.omit(as.data.frame(p)))
      if(precise < 0.5) next
      r = data.frame(n=n,sep=sep,precise=precise)
      result = rbind(result,r)
    }
  }
  
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#a1para = seq(0.01,0.09,0.01) a2para = seq(0.1,0.5,0.1)
optimSAR = function(pricedata,analysedata,start,end,a1para,a2para)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(ac1 in a1para)
  {
    for(ac2 in a2para)
    {
      signal = getSARsignal(pricedata,ac1,ac2,period)
      colnames(signal) ='signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
#       
#       a1 = tb$tables[[1]][1,1]
#       a2 = tb$tables[[1]][2,1]
#       #同时大于或小于0.5 则不考虑
#       if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #if(a1 > 0.5 || a2 < 0.5) next
     ##if(abs(a1-a2) < 0.03) next
      #找出距离最大的参数
      precise =  fitness(tb,na.omit(as.data.frame(p)))
      if(precise < 0.5) next
      r = data.frame(ac1=ac1,ac2=ac2,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#npara = seq(3,15,2) uppara = seq(0.6,0.9,0.1) downpara = seq(0.4,0.1,-0.1)
optimWPR = function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(up in uppara)
    {
      for(down in downpara)
      {
        signal = getWPRsignal(pricedata,n,up,down,period)
        colnames(signal) ='signal'
        p = merge(analysedata_train[,'leadclflag'],signal)
        p[p=='hold'] = NA
        
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        
#         a1 = tb$tables[[1]][1,1]
#         a2 = tb$tables[[1]][2,1]
#         #同时大于或小于0.5 则不考虑
#         if(sign(a1-0.5) == sign(a2 - 0.5)) next
        #if(a1 > 0.5 || a2 < 0.5) next
      ##if(abs(a1-a2) < 0.03) next
        #找出距离最大的参数
        precise =  fitness(tb,na.omit(as.data.frame(p)))
        if(precise < 0.5) next
        r = data.frame(n=n,up=up,down=down,precise=precise)
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#nfkpara = seq(5,20,2) nfdpara=seq(3,11,2) nsdpara = seq(3,11,2)
# 类似smi指标
optimKDJ = function(pricedata,analysedata,start,end,nfkpara,nfdpara,nsdpara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(nfk in nfkpara)
  {
    for(nfd in nfdpara)
    {
      for(nsd in nsdpara)
      {
        signal = getKDJsignal(pricedata,nfk,nfd,nsd,period)
        colnames(signal) = 'signal'
        
        p = merge(analysedata_train[,'leadclflag'],signal)
        p[p=='hold'] = NA
        
        #有效信号占比小于10% 过滤
        effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
        if(effecratio < 0.1) next
        
         tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
#         
#         a1 = tb$tables[[1]][1,1]
#         a2 = tb$tables[[1]][2,1]
        #同时大于或小于0.5 则不考虑
        #if(sign(a1-0.5) == sign(a2 - 0.5)) next
        #if(a1 > 0.5 || a2 < 0.5) next
      #if(abs(a1-a2) < 0.03) next
        #找出距离最大的参数
        precise =  fitness(tb,na.omit(as.data.frame(p)))
        if(precise < 0.5) next
        r = data.frame(nfk=nfk,nfd=nfd,nsd=nsd,precise=precise)
        result = rbind(result,r)
      }
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)  
}

#smi 同 kdj类似

# npara = seq(3,20,2) seppara =seq(-10,10,5)
optimTDI = function(pricedata,analysedata,start,end,npara,seppara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sep in seppara)
    {
      signal = getTDIsignal(pricedata,n,sep,period)
      colnames(signal) = 'signal'
      
      p = merge(analysedata_train[,'leadclflag'],signal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
#       a1 = tb$tables[[1]][1,1]
#       a2 = tb$tables[[1]][2,1]
#       #同时大于或小于0.5 则不考虑
#       if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #if(a1 > 0.5 || a2 < 0.5) next
    #if(abs(a1-a2) < 0.03) next
      #找出距离最大的参数
      precise =  fitness(tb,na.omit(as.data.frame(p)))
      if(precise < 0.5) next
      r = data.frame(n=n,sep=sep,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#npara = seq(3,20,2)
optimKST = function(pricedata,analysedata,start,end,npara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    signal = getKSTsignal(pricedata,n,period)
    colnames(signal) = 'signal'
    
    p = merge(analysedata_train[,'leadclflag'],signal)
    p[p=='hold'] = NA
    
    #有效信号占比小于10% 过滤
    effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
    if(effecratio < 0.1) next
    
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    
#     a1 = tb$tables[[1]][1,1]
#     a2 = tb$tables[[1]][2,1]
#     #同时大于或小于0.5 则不考虑
#     if(sign(a1-0.5) == sign(a2 - 0.5)) next
    #if(a1 > 0.5 || a2 < 0.5) next
  #if(abs(a1-a2) < 0.03) next
    #找出距离最大的参数
    precise =  fitness(tb,na.omit(as.data.frame(p)))
    if(precise < 0.5) next
    r = data.frame(n=n,precise=precise)
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#无参数优化
optimChaikinAD = function(pricedata,analysedata,start,end)
{
  period = paste(start,end,sep='/')
  chaikinadsignal = getChaikinsignal(pricedata,period)
  effecratio = length(chaikinadsignal[chaikinadsignal!='hold']) / length(chaikinadsignal)
  if(effecratio < 0.1) return(NA)
  else return(data.frame(1))
}

# seppara = seq(10,-10,-5)
optimOBV = function(pricedata,analysedata,start,end,seppara)
{
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(sep in seppara)
  {
    signal = getOBVsignal(pricedata,sep,period)
    colnames(signal) = 'signal'
    
    p = merge(analysedata_train[,'leadclflag'],signal)
    p[p=='hold'] = NA
    
    #有效信号占比小于10% 过滤
    effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
    if(effecratio < 0.1) next
    
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    
#     a1 = tb$tables[[1]][1,1]
#     a2 = tb$tables[[1]][2,1]
#     #同时大于或小于0.5 则不考虑
#     if(sign(a1-0.5) == sign(a2 - 0.5)) next
    #if(a1 > 0.5 || a2 < 0.5) next
  #if(abs(a1-a2) < 0.03) next
    #找出距离最大的参数
    precise =  fitness(tb,na.omit(as.data.frame(p)))
    if(precise < 0.5) next
    r = data.frame(sep=sep,precise=precise)
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#npara = seq(3,20,2) n1para = seq(3,20,2)
optimCMO = function(pricedata,analysedata,start,end,npara,n1para)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(n1 in n1para)
    {
      signal = getCMOsignal(pricedata,n,n1,period)
      colnames(signal) = 'signal'
      
      p = merge(analysedata_train[,'leadclflag'],signal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
#       
#       a1 = tb$tables[[1]][1,1]
#       a2 = tb$tables[[1]][2,1]
#       #同时大于或小于0.5 则不考虑
#       if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #if(a1 > 0.5 || a2 < 0.5) next
    #if(abs(a1-a2) < 0.03) next
      #找出距离最大的参数
      precise =  fitness(tb,na.omit(as.data.frame(p)))
      if(precise < 0.5) next
      r = data.frame(n=n,n1=n1,precise=precise)
      result = rbind(result,r)
      
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

#npara = seq(3,21,2) seppara = seq(10,-10,-5)
optimCMF = function(pricedata,analysedata,start,end,npara,seppara)
{
 
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sep in seppara)
    {
      signal = getCMFsignal(pricedata,n,sep,period)
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
#       a1 = tb$tables[[1]][1,1]
#       a2 = tb$tables[[1]][2,1]
#       #同时大于或小于0.5 则不考虑
#       if(sign(a1-0.5) == sign(a2 - 0.5)) next
  #if(a1 > 0.5 || a2 < 0.5) next
  #if(abs(a1-a2) < 0.03) next
      #找出距离最大的参数
      precise =  fitness(tb,na.omit(as.data.frame(p)))
      if(precise < 0.5) next
      r = data.frame(n=n,sep=sep,precise=precise)
      result = rbind(result,r)
      
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

# npara = seq(3,21,2) seppara = seq(10,-10,-5)
optimEMV = function(pricedata,analysedata,start,end,npara,seppara)
{
  
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sep in seppara)
    {
      signal = getEMVsignal(pricedata,n,sep,period) 
      colnames(signal) = 'signal'
      
      p = merge(analysedata_train[,'leadclflag'],signal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
#       a1 = tb$tables[[1]][1,1]
#       a2 = tb$tables[[1]][2,1]
#       #同时大于或小于0.5 则不考虑
#       if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #if(a1 > 0.5 || a2 < 0.5) next
    #if(abs(a1-a2) < 0.03) next
      #找出距离最大的参数
      precise =  fitness(tb,na.omit(as.data.frame(p)))
      if(precise < 0.5) next
      r = data.frame(n=n,sep=sep,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
   
}

# npara = seq(3,21,2) sigpara = seq(3,10,2)
optimTRIX = function(pricedata,analysedata,start,end,npara,sigpara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sig in sigpara)
    {
      signal = getTRIXsignal(pricedata,n,sig,period)
      colnames(signal) = 'signal'
      
      p = merge(analysedata_train[,'leadclflag'],signal)
      p[p=='hold'] = NA
      
      #有效信号占比小于10% 过滤
      effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
      if(effecratio < 0.1) next
      
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      
#       a1 = tb$tables[[1]][1,1]
#       a2 = tb$tables[[1]][2,1]
#       #同时大于或小于0.5 则不考虑
#       if(sign(a1-0.5) == sign(a2 - 0.5)) next
      #if(a1 > 0.5 || a2 < 0.5) next
    #if(abs(a1-a2) < 0.03) next
      #找出距离最大的参数
      precise =  fitness(tb,na.omit(as.data.frame(p)))
      if(precise < 0.5) next
      r = data.frame(n=n,sig=sig,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

# seppara = seq(10,-10,-5)
optimWilliamsAD = function(pricedata,analysedata,start,end,seppara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  
  for(sep in seppara)
  {
    signal = getWilliamADsignal(pricedata,sep,period)
    colnames(signal) = 'signal'
    
    p = merge(analysedata_train[,'leadclflag'],signal)
    p[p=='hold'] = NA
    
    #有效信号占比小于10% 过滤
    effecratio = length(p[,2][!is.na(p[,2])]) / length(p[,2])
    if(effecratio < 0.1) next
    
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    
#     a1 = tb$tables[[1]][1,1]
#     a2 = tb$tables[[1]][2,1]
#     #同时大于或小于0.5 则不考虑
#     if(sign(a1-0.5) == sign(a2 - 0.5)) next
    #if(a1 > 0.5 || a2 < 0.5) next
  #if(abs(a1-a2) < 0.03) next
    #找出距离最大的参数
    precise =  fitness(tb,na.omit(as.data.frame(p)))
    if(precise < 0.5) next
    r = data.frame(sep=sep,precise=precise)
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}