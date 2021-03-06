readOneStock = function(code)
{
  require('quantmod')
  path = "D:/data/stock/dest"
  files = dir(path)
  f = grep(code,files,value=T)
  if(length(f) != 1 ) return(NULL)
  fname = file.path(path,f)
  pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",index.column=1) 
  colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
  time(pricedata)=as.POSIXct(time(pricedata))
  pricedata=as.xts(pricedata)
  return(pricedata)
}

readOnestockweek = function(code,shindex,codeTable)
{
  require('dplyr')
  path = "D:/data/stock/dest"
  files = dir(path)
  f = grep(code,files,value=T)
  fname = file.path(path,f)
  pricedata = readOneStock(code) 
  colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
  time(pricedata)=as.POSIXct(time(pricedata))
  pricedata=as.xts(pricedata)
  pricedata = to.weekly(pricedata)
  colnames(pricedata)<-c("Open","High","Low","Close","Volume")
  pricedata$sma30 = SMA(Cl(pricedata),n=30)
  pricedata$volatile = (Cl(pricedata)-Op(pricedata))/Op(pricedata)
  pricedata$atr = ATR(HLC(pricedata),n=5)
  
  pricedata = na.omit(pricedata)
  pricedata$stage = judegeStage(pricedata$sma30)
  pricedata = na.omit(pricedata)
  pricedata$meanVolume = apply.weekly(pricedata[,'Volume'],mean)
  pricedata$mvSma10 = lag(SMA(pricedata$meanVolume,10),1)
  pricedata$mvratio = pricedata$meanVolume / pricedata$mvSma10 
  rs = RS(Cl(shindex),Cl(pricedata))
  rs[which(rs==Inf | rs == -Inf)] = 0
  
  pricedata$rs = rs
  pricedata$rsSma10 =lag(SMA(rs,10),1)
  pricedata$rsratio = pricedata$rs / pricedata$rsSma10 
  
  fname = strsplit(f,'.',fixed=T)[[1]][1]
  fname = substr(fname,3,8)
  
  hy = filter(codeTable,stockcode==fname)
  hycode = hy[1,'hycode']
  #找不到对应代码
  if(nrow(hy) == 0 || !exists(hycode))
  {
    pricedata$hy = NA
    pricedata$hystage = NA
    pricedata$hyrs = NA
    pricedata$hyrsSma10 = NA
    pricedata$hyrsRatio = NA
    
  }
  else
  {
    pricedata$hy = hycode
    hy = get(hycode,envir = e)
    hystage = hy[,'stage']
    pricedata$hystage = hystage
    pricedata$hyrs = hy[,'rs']
    pricedata$hyrsSma10 =hy[,'rsSma10']
    pricedata$hyrsRatio =hy[,'rsratio']
    
  }
  return(pricedata)
}
