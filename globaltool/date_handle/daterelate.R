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
}