readOneStock = function(code)
{
  require('quantmod')
  path = "D:/data/stock/dest"
  files = dir(path)
  f = grep(code,files,value=T)
  if(length(f) == 0 ) return(NULL)
  fname = file.path(path,f)
  pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",index.column=1) 
  colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
  time(pricedata)=as.POSIXct(time(pricedata))
  pricedata=as.xts(pricedata)
  return(pricedata)
}