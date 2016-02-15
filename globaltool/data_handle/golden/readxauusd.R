readxauusd = function(period='days')
{
  require('quantmod')
  path = "D:/data/黄金/xauusd.csv"
  pricedata = read.zoo(path,header=T, format = "%Y/%m/%d",sep=",",index.column=1) 
  return(to.period(pricedata,period=period))
}