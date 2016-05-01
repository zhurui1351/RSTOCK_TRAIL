require(quantmod)
read_dou1_ch = function(path = 'D:/data/commidity/soybean/d/')
{
  fname = paste(path,'dou1day.txt',sep='')
  pricedata = read.zoo(fname,header=F, format = "%m/%d/%Y",sep=",",index.column=1) 
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

read_s1_quandl = function(path = 'D:/data/collectdata/quandldata/commidity/soybean/CME/')
{
  fname = paste(path,'S1.txt',sep='')
  pricedata = read.csv(fname,header=T,sep=",") 
  pricedata = xts(pricedata[,2:7],order.by = as.Date(pricedata$Date))
  
}

read_dou1_d_wind = function(path = 'D:/data/collectdata/windata/commidity/soybean/DCE/')
{
  fname = paste(path,'dou1day.csv',sep='')
  pricedata = read.csv(fname,header=T,sep=",") 
  pricedata = xts(pricedata[,2:10],order.by = as.Date(pricedata$DATETIME))
  colnames(pricedata) = c('Open','High','Low','Close','Vol','Amt','Dealnum','Settle','Oi')
  return(pricedata)
}

read_s1_d_wind = function(path = 'D:/data/collectdata/windata/commidity/soybean/CME/')
{
  fname = paste(path,'s1_wind.csv',sep='')
  pricedata = read.csv(fname,header=T,sep=",") 
  pricedata = xts(pricedata[,2:10],order.by = as.Date(pricedata$DATETIME))
  colnames(pricedata) = c('Open','High','Low','Close','Vol','Amt','Dealnum','Settle','Oi')
  return(pricedata)
}

read_s1_1m_taobao = function(path = 'D:/data/collectdata/taobaodata/commidity/soybean/')
{
  fname = paste(path,'dou1_1m_taobao.csv',sep='')
  pricedata = read.csv(fname,header=T,sep=",") 
  pricedata = xts(pricedata[,2:7],order.by = as.POSIXct(pricedata[,1]))
  colnames(pricedata) = c('Open','High','Low','Close','Vol','Oi')
  return(pricedata)
}