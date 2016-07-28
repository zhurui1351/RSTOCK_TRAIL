require(RMySQL)
conn_td <- dbConnect(MySQL(), dbname = "t_d", username="root", password="123456",host="10.101.0.13",port=3306)
conn_td_report <- dbConnect(MySQL(), dbname = "t_d_report", username="root", password="123456",host="10.101.0.13",port=3306)

dbListTables(conn_td_report)

dbReadTable(conn_td_report, "test")

dbSendQuery(conn_td_report,'SET NAMES utf8')

testdata = data.frame(name='祝睿',fee=10.1,date=NA)
dbWriteTable(conn_td_report, "test3", testdata,append=T,row.names=F)


dbWriteTable(conn_td_report, "test1", weixindt,overwrite = T,row.names=F)


survrate_dt = data.frame(date = as.Date(sur_date),rates = surv_rates)
dbWriteTable(conn_td_report, "survrates", survrate_dt,append=T,row.names=F)

dbDisconnect(conn_td)
dbDisconnect(conn_td_report)