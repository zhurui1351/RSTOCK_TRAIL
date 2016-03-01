getFxdata_wind = function()
{
  path='D:/data/collectdata/windata/forex/usdcny_d.csv'
  today = as.character(as.Date(now()))
  w_wsd_data<-w.wsd("USDCNY.EX","close","1995-01-01",today,"Fill=Previous;PriceAdj=F")
  data = w_wsd_data$Data
  write.csv(data,path,row.names=F,quote=F)
}