readusdcny_d = function(path = 'D:/data/collectdata/windata/forex/usdcny_d.csv')
{
  pricedata = read.csv(path,header=T,sep=",") 
  pricedata = xts(pricedata[,2],order.by = as.Date(pricedata[,1]))
  colnames(pricedata) = c('Close')
  return(pricedata)
}