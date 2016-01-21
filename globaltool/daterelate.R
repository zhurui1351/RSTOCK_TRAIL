# d like '2015-02'
getMonthEnd = function(d)
{
  require('lubridate')
  nd = paste(d,'01',sep='-') 
  nd = ymd(nd) + months(1)
  nd = nd  - days(1)
  return( as.Date(nd))
}