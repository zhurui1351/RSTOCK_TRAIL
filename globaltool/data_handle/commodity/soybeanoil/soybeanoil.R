read_y_1m_taobao = function(path = 'D:/data/collectdata/taobaodata/commidity/soybeanoil/')
{
  fname = paste(path,'douyou_1m_taobao.csv',sep='')
  pricedata = read.csv(fname,header=T,sep=",") 
  pricedata = xts(pricedata[,2:7],order.by = as.POSIXct(pricedata[,1]))
  colnames(pricedata) = c('Open','High','Low','Close','Vol','Oi')
  return(pricedata)
}