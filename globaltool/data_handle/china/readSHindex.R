readSHindex = function()
{
  path = "D:/data/stock/index/dest"
  #Sys.setenv(TZ="UTC")
  f='SH000001.TXT'
  fname = file.path(path,f)
  shindex = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",index.column=1) 
  colnames(shindex)<-c("Open","High","Low","Close","Volume","Amount")
  time(shindex)=as.POSIXct(time(shindex))
  shindex=as.xts(shindex)
  return(shindex)
}
