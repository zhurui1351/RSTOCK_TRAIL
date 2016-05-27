# d like '2015-02'
getMonthEnd = function(d)
{
  require('lubridate')
  nd = paste(d,'01',sep='-') 
  nd = ymd(nd) + months(1)
  nd = nd  - days(1)
  return(as.Date(nd))
}

#大陆期货夜盘，分钟转日处理函数
to_day = function(pricedata)
{
  days = as.character(unique(as.Date(index(pricedata))))
  day = days[1]
  price = pricedata[day]
  open = as.numeric(price[1,]$Open)
  high = max(as.numeric(price$High))
  low = min(as.numeric(price$Low))
  close = tail(price,1)$Close
  vol = sum(as.numeric(price$Vol))
  oi = tail(price,1)$Oi
  rs = xts(data.frame(Open=open,High=high,Low=low,Close=close,Vol=vol,Oi=oi), order.by = as.Date(day))
  for(i in 2:length(days))
  {
    preday = days[i-1]
    day = days[i]
    
    starttime = paste(preday,'21:00:00')
    endtime = paste(day,'15:00:00')
    timespan = paste(starttime,endtime,sep='/')
    price = pricedata[timespan]
    open = as.numeric(price[1,]$Open)
    high = max(as.numeric(price$High))
    low = min(as.numeric(price$Low))
    close = tail(price,1)$Close
    vol = sum(as.numeric(price$Vol))
    oi = tail(price,1)$Oi
    r = xts(data.frame(Open=open,High=high,Low=low,Close=close,Vol=vol,Oi=oi), order.by = as.Date(day))
    rs = rbind(rs,r)
  }
  return(rs)
}

#处理后对齐的数据
to_minutes = function(pricedata,k=5)
{
  times = index(pricedata)
  i = 1
  newtime =  times[i]
  ps = pricedata[i:(i+k-1)]
  open = as.numeric(ps[1,]$Open)
  high = max(as.numeric(ps$High))
  low = min(as.numeric(ps$Low))
  close = tail(ps,1)$Close
  vol = sum(as.numeric(ps$Vol))
  oi = tail(ps,1)$Oi
  r = xts(data.frame(Open=open,High=high,Low=low,Close=close,Vol=vol,Oi=oi), order.by = newtime)
  rs = r
  i = i + k
  while(i < length(times))
  {
    newtime =  times[i]
    ps = pricedata[i:(i+k-1)]
    open = as.numeric(ps[1,]$Open)
    high = max(as.numeric(ps$High))
    low = min(as.numeric(ps$Low))
    close = tail(ps,1)$Close
    vol = sum(as.numeric(ps$Vol))
    oi = tail(ps,1)$Oi
    r = xts(data.frame(Open=open,High=high,Low=low,Close=close,Vol=vol,Oi=oi), order.by = newtime)
    rs = rbind(rs,r)
    i = i + k
  }
}