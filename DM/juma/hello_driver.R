require(RMySQL)

dbname_td = "tgm"
dbname_td_report = 'tgm_report'
username="wangchao"
password = 'juma9156'
host = '10.101.0.13'
port = 3306
conn_tgm <- dbConnect(MySQL(), dbname = dbname_td, username=username, password=password,host=host,port=port)
dbSendQuery(conn_tgm,'SET NAMES gbk')
sql = 'SELECT
  `waybill_id`,
`status`,
`is_delete`,
`create_time`,
`create_user_id`
FROM `tgm`.`waybill`
WHERE STATUS<10 AND is_delete = 0'
orderdt = dbGetQuery(conn_tgm,sql)
orderdt$create_date = as.Date(orderdt$create_time)
dbDisconnect(conn_tgm)
