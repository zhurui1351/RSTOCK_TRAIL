fitness = function(tb,p)
{
  pr = predict(tb, as.data.frame(p))
  tt = table(pr, p[,'leadclflag'])
  precise = (tt[1,1]+tt[2,2]) / sum(tt)
  return(precise)
}

optimClosestatus = function(pricedata,analysedata,start,end,npara)
{
  period = paste(start,end,sep='/')
  result = data.frame()
  analysedata_train = analysedata[period]
  #遍历寻找最优解
  for( n in npara)
  {
    signal = getClosestatus(pricedata,n,period)
    colnames(signal) = 'signal'
    p = merge(analysedata_train[,'leadclflag'],signal)
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    precise = fitness(tb,p)
    if(precise < 0.5) next
    r = data.frame(n=n,precise=precise)
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimSMAstatus = function(pricedata,analysedata,start,end,shortpara,longpara)
{
  period = paste(start,end,sep='/')
  result = data.frame()
  analysedata_train = analysedata[period]
  #遍历寻找最优解
  for(long in longpara)
  {
    for( short in shortpara)
    {
      if(short > long) next
      signal = getSMAstatus(pricedata,short,long,period)
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      precise = fitness(tb,p)
      if(precise < 0.5) next
      r = data.frame(short=short,long=long,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}


optimCCIstatus = function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
  period = paste(start,end,sep='/')
  result = data.frame()
  analysedata_train = analysedata[period]
  #遍历寻找最优解
  for(n in npara)
  {
    for(up in uppara)
    {
      for( down in downpara)
      {
        signal = getCCIstatus(pricedata,n,up,down,period)
        colnames(signal) = 'signal'
        p = merge(analysedata_train[,'leadclflag'],signal)
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        precise = fitness(tb,p)
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

optimRSIstatus = function(pricedata,analysedata,start,end,npara,uppara,downpara)
{
  period = paste(start,end,sep='/')
  result = data.frame()
  analysedata_train = analysedata[period]
  #遍历寻找最优解
  for(n in npara)
  {
    for(up in uppara)
    {
      for( down in downpara)
      {
        signal = getRSIstatus(pricedata,n,up,down,period)
        colnames(signal) = 'signal'
        p = merge(analysedata_train[,'leadclflag'],signal)
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        precise = fitness(tb,p)
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

optimMACDstatus = function(pricedata,analysedata,start,end,nfastpara,nslowpara,nsigpara,seppara)
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
          signal = getMACDstatus(pricedata,nfast,nslow,nsig,sep,period)
          colnames(signal) = 'signal'
          p = merge(analysedata_train[,'leadclflag'],signal)
          tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
          precise = fitness(tb,p)
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

optimADXstatus = function(pricedata,analysedata,start,end,npara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    signal = getADXstatus(pricedata,n,period)
    colnames(signal) = 'signal'
    p = merge(analysedata_train[,'leadclflag'],signal)
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    precise = fitness(tb,p)
    if(precise < 0.5) next
    r = data.frame(n=n,precise=precise)
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimMFIstatus=function(pricedata,analysedata,start,end,npara,uppara,downpara)
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
        signal = getMFIstatus(pricedata,n,up,down,period)
        colnames(signal) = 'signal'
        p = merge(analysedata_train[,'leadclflag'],signal)
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        precise = fitness(tb,p)
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

optimBBANDSstatus = function(pricedata,analysedata,start,end,npara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    
    signal = getBBANDSstatus(pricedata,n,period)
    colnames(signal) = 'signal'
    p = merge(analysedata_train[,'leadclflag'],signal)
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    precise = fitness(tb,p)
    if(precise < 0.5) next
    r = data.frame(n=n,precise=precise)
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimROCstatus = function(pricedata,analysedata,start,end,npara,seppara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  
  for(n in npara)
  {
    for(sep in seppara)
    {
      signal = getROCstatus(pricedata,n,sep,period)
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      precise = fitness(tb,p)
      if(precise < 0.5) next
      r = data.frame(n=n,sep=sep,precise=precise)
      result = rbind(result,r)
    }
  }
  
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimSARstatus = function(pricedata,analysedata,start,end,a1para,a2para)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(ac1 in a1para)
  {
    for(ac2 in a2para)
    {
      signal = getSARstatus(pricedata,ac1,ac2,period)
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      precise = fitness(tb,p)
      if(precise < 0.5) next
      r = data.frame(ac1=ac1,ac2=ac2,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}


optimWPRstatus = function(pricedata,analysedata,start,end,npara,uppara,downpara)
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
        signal = getWPRstatus(pricedata,n,up,down,period)
        colnames(signal) = 'signal'
        p = merge(analysedata_train[,'leadclflag'],signal)
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        precise = fitness(tb,p)
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

optimKDJstatus = function(pricedata,analysedata,start,end,nfkpara,nfdpara,nsdpara)
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
        signal = getKDJstatus(pricedata,nfk,nfd,nsd,period)
        colnames(signal) = 'signal'
        p = merge(analysedata_train[,'leadclflag'],signal)
        tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
        precise = fitness(tb,p)
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


optimTDIstatus = function(pricedata,analysedata,start,end,npara,seppara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sep in seppara)
    {
      signal = getTDIstatus(pricedata,n,sep,period)
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      precise = fitness(tb,p)
      if(precise < 0.5) next
      r = data.frame(n=n,sep=sep,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}


optimKSTstatus = function(pricedata,analysedata,start,end,npara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    signal = getKSTstatus(pricedata,n,period)
    colnames(signal) = 'signal'
    p = merge(analysedata_train[,'leadclflag'],signal)
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    precise = fitness(tb,p)
    if(precise < 0.5) next
    r = data.frame(n=n,precise=precise)
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimChaikinVostatus = function(pricedata,analysedata,start,end,npara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    signal = getChaikinVostatus(pricedata,n,period)
    colnames(signal) = 'signal'
    p = merge(analysedata_train[,'leadclflag'],signal)
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    precise = fitness(tb,p)
    if(precise < 0.5) next
    r = data.frame(n=n,precise=precise)
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimOBVstatus = function(pricedata,analysedata,start,end,npara)
{
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    signal = getOBVstatus(pricedata,n,period)
    colnames(signal) = 'signal'
    p = merge(analysedata_train[,'leadclflag'],signal)
    tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
    precise = fitness(tb,p)
    if(precise < 0.5) next
    r = data.frame(n=n,precise=precise)
    result = rbind(result,r)
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimCMOstatus = function(pricedata,analysedata,start,end,npara,n1para)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(n1 in n1para)
    {
      signal = getCMOstatus(pricedata,n,n1,period)
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      precise = fitness(tb,p)
      if(precise < 0.5) next
      r = data.frame(n=n,n1=n1,precise=precise)
      result = rbind(result,r)
      
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimCMFstatus = function(pricedata,analysedata,start,end,npara,seppara)
{
  
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sep in seppara)
    {
      signal = getCMFstatus(pricedata,n,sep,period)
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      precise = fitness(tb,p)
      if(precise < 0.5) next
      r = data.frame(n=n,sep=sep,precise=precise)
      result = rbind(result,r)
      
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimEMVstatus = function(pricedata,analysedata,start,end,npara,seppara)
{
  
  if(!has.Vo(pricedata)) return(NA)
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sep in seppara)
    {
      signal = getEMVstatus(pricedata,n,sep,period) 
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      precise = fitness(tb,p)
      if(precise < 0.5) next
      r = data.frame(n=n,sep=sep,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}

optimTRIXstatus = function(pricedata,analysedata,start,end,npara,sigpara)
{
  period = paste(start,end,sep='/')
  analysedata_train = analysedata[period]
  result = data.frame()
  for(n in npara)
  {
    for(sig in sigpara)
    {
      signal = getTRIXstatus(pricedata,n,sig,period)
      colnames(signal) = 'signal'
      p = merge(analysedata_train[,'leadclflag'],signal)
      tb = naiveBayes(leadclflag~signal,data=as.data.frame(p))
      precise = fitness(tb,p)
      if(precise < 0.5) next
      r = data.frame(n=n,sig=sig,precise=precise)
      result = rbind(result,r)
    }
  }
  if(nrow(result) == 0) return(NA)
  s = subset(result,precise==max(result$precise))[1,]
  return(s)
}
