require(quantmod)
require(TTR)
library(blotter)

path = "D:/data/dest"
#Sys.setenv(TZ="UTC")
files = dir(path)
#files=c('SH603606.TXT')
f = files[100]
for(f in files)
{
  fname = file.path(path,f)
  pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",index.column=1) 
  colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
  time(pricedata)=as.POSIXct(time(pricedata))
  pricedata=as.xts(pricedata)
  pricedata$volatile = (Cl(pricedata)-Op(pricedata))/Op(pricedata)
}

dates = time(pricedata)
for(i in 1 : length(dates))
{
  if(as.numeric(pricedata[i,]$volatile) > 0.06)
  {
    print(pricedata[i:(i+6),])
    print("next")
  }
}
