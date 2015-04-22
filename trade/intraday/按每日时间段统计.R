pricedata = to.minutes30(priceData)
time(pricedata) = as.POSIXct(strftime(time(pricedata)),tz='Etc/GMT-8')
daydata = to.daily(priceData)
daydate = time(daydata)
from = "20:00:00"
to = "04:00:00"
v = c()
isCrossday = T
iternums = nrow(daydata)Z
for(day in 1:iternums)
{
  if(isCrossday == F)
  {
    date = as.character(daydate[day])
    timefrom = paste(date,from)
    timeto = paste(date,to)
    timeperiod = paste(timefrom,timeto,sep="/")
  }
  else #跨日
  {
    if(day+1 >iternums) next;
    date = as.character(daydate[day])
    date1 = as.character(daydate[day+1])
    timefrom = paste(date,from)
    timeto = paste(date1,to)
    timeperiod = paste(timefrom,timeto,sep="/")
  }
  m_data = priceData[timeperiod]
  if(nrow(m_data) == 0) next
  colnames(m_data) <- c("Open","High","Low","Close","Volume")
  votality = max(Hi(m_data)) - min(Lo(m_data))
  v = c(v,votality)
  
}