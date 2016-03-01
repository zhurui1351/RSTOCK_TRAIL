require(WindR)

#主力合约代码
#w_wsd_code<-w.wsd("A.DCE","trade_hiscode","2014-06-15","2015-12-15")

getA1_d = function()
{
  path = 'D:/data/collectdata/windata/commidity/soybean/DCE/dou1day.csv'
  w_data<-w.wsd("A.DCE","open,high,low,close,volume,amt,dealnum,settle,oi","1999-01-01","2016-02-29","Currency=CNY;PriceAdj=F")
  pricedata = w_data$Data
  write.csv(pricedata,path,row.names=F,quote=F)
}

getA1_1m = function()
{
  path = 'D:/data/collectdata/windata/commidity/soybean/DCE/dou11m.csv'
  w_wsi_data<-w.wsi("A.DCE","open,high,low,close,volume,amt,oi","2012-01-01 09:00:00","2016-02-29 18:42:00","PriceAdj=F")  
  pricedata = na.omit(w_wsi_data$Data)
  write.csv(pricedata,path,row.names=F,quote=F)
  
}