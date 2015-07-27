readallHy = function()
{
  print(now())
  e = parent.env(environment())
  lookups = c()
  indexlookups =  1
  path = "D:/data/stock/hyindex/dest"
  files = dir(path)
  rm(list=files,envir=e)
  lookups = c()
  indexlookups =  1
  
  for(f in files)
  {
    #print(f)
    fname = file.path(path,f)
    pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",index.column=1) 
    if(nrow(pricedata) < 500){ next}
    colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
    time(pricedata)=as.POSIXct(time(pricedata))
    pricedata=as.xts(pricedata)
    pricedata = to.weekly(pricedata)
    colnames(pricedata)<-c("Open","High","Low","Close","Volume")
    pricedata$sma30 = SMA(Cl(pricedata),n=30)
    pricedata$volatile = (Cl(pricedata)-Op(pricedata))/Op(pricedata)
    
    pricedata = na.omit(pricedata)
    pricedata$stage = judegeStage(pricedata$sma30)
    pricedata = na.omit(pricedata)
    pricedata$meanVolume = apply.weekly(pricedata[,'Volume'],mean)
    
    fname = strsplit(f,'.',fixed=T)[[1]][1]
    fname = substr(fname,3,8)
    
    assign(fname,pricedata,envir=e)
    lookups[indexlookups] =fname
    indexlookups = indexlookups + 1
  } 
  print(now())
  return(lookups)
}