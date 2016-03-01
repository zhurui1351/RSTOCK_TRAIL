getshindex= function()
{
  path = 'D:/data/collectdata/windata/index/sh000001.csv'
  w_data<-w.wsd("000001.sh","open,high,low,close,volume,amt,dealnum,settle,oi","1990-01-01 09:00:00","2016-02-29","PriceAdj=F")  
  pricedata = w_data$Data
  write.csv(pricedata,path,row.names=F,quote=F)
}