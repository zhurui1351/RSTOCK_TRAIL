require(quantmod)
require(TTR)
require('dygraphs')
require('lubridate')
require('dplyr')
path = "D:/data/index"
#Sys.setenv(TZ="UTC")
f='SH000001.TXT'
fname = file.path(path,f)
shindex = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",index.column=1) 
colnames(shindex)<-c("Open","High","Low","Close","Volume","Amount")
time(shindex)=as.POSIXct(time(shindex))
shindex=as.xts(shindex)

path = "D:/data/dest"
f = "SH600000.TXT"
fname = file.path(path,f)
pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",index.column=1) 
colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
time(pricedata)=as.POSIXct(time(pricedata))
pricedata=as.xts(pricedata)

shdeltratio = na.omit(Delt(Cl(shindex)) * 100)
pdeltratio = na.omit(Delt(Cl(pricedata)) * 100)

rsdata = na.omit(merge(shdeltratio,pdeltratio))

rs = rsdata[,2] / rsdata[,1]
