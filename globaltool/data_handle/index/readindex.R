read_sh000001 = function(path='D:/data/collectdata/windata/index/')
{
  fname = paste(path,'sh000001.csv',sep='')
  pricedata = read.csv(fname,header=T,sep=",") 
  pricedata = xts(pricedata[,2:8],order.by = as.Date(pricedata[,1]))
  colnames(pricedata) = c('Open','High','Low','Close','Vol','Amt','Dealnum')
  return(pricedata)
}