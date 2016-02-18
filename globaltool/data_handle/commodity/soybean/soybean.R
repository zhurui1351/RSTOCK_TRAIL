require(quantmod)
read_dou1_ch = function(path = 'D:/data/commidity/soybean/d/')
{
  fname = paste(path,'dou1day.txt',sep='')
  pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",index.column=1) 
  colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount","settle")
  return(as.xts(pricedata))
}

read_dou2_ch = function(path = 'D:/data/commidity/soybean/d/')
{
  fname = paste(path,'dou2day.txt',sep='')
  pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",index.column=1) 
  colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount","settle")
  return(as.xts(pricedata))
}

read_s_f_us = function(path = 'D:/data/commidity/soybean/d/')
{
  fname = paste(path,'s_f_d.csv',sep='')
  pricedata = read.csv(fname,header=T,sep=",") 
  pricedata = xts(pricedata[,2:7],order.by = as.Date(pricedata$Date))
  return(as.xts(pricedata))
}