readxauusd = function(period='days')
{
  require('quantmod')
  path = "D:/data/原油/cl_f_d.csv"
  pricedata = read.csv(path,header=T)
  pricedata = xts(pricedata[,2:7],order.by = as.Date(pricedata$Date))
  return(to.period(pricedata,period=period))
}
