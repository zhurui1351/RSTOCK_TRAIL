rm(list = ls(all=T))
require(RMySQL)
require(lubridate)
source('D:/Rcode/code/RSTOCK_TRAIL/DM/juma/car_fun_online.R',encoding = 'utf8')

conn_td <- dbConnect(MySQL(), dbname = "t_d", username="wangchao", password="juma9156",host="10.101.0.13",port=3306)
conn_td_report <- dbConnect(MySQL(), dbname = "t_d_report", username="wangchao", password="juma9156",host="10.101.0.13",port=3306)

sql = 'SELECT create_time,customer_id,price,artificer_id FROM t_d.order_info WHERE STATUS IN (4,5,6,7)'
orderdt = dbGetQuery(conn_td,sql)
orderdt$create_date = as.Date(orderdt$create_time)
sql = 'select id ,create_time from customer'
cusdt = dbGetQuery(conn_td,sql)
cusdt$create_date = as.Date(cusdt$create_time)
dbDisconnect(conn_td)
dbDisconnect(conn_td_report)
#每月拉新,订单量,流水
cus_date = cusdt$create_time
cus_date_m = strftime(cus_date,format = '%Y-%m')
cus_dt_m = aggregate(cus_date_m,by = list(cus_date_m),length)

order_date = orderdt$create_time
order_date_m = strftime(order_date,format = '%Y-%m')
order_dt_m = aggregate(order_date_m,by = list(order_date_m),length)

fee_dt_m = aggregate(as.numeric(orderdt$price),by = list(order_date_m),function(x){sum(x,na.rm=T)})

#report_m_1[,1] = paste(report_m_1[,1],'-01',sep='')
#report_m_1$date = as.Date(report_m_1$date)
#留存、月活
all_m = unique(order_date_m)
surv_rates = data.frame()
mon_live = data.frame()
for(i in 1:(length(all_m)-1))
{
  m = all_m[i]
  d = paste(m,'-01',sep='')
  start = as.Date(d)
  endday = as.Date(d) + months(1) - days(1)
  survrate = survival_rate(cus_flag_func(cusdt,orderdt,endday))
  survrate = ifelse(length(survrate) ==0,0,survrate)
  r = data.frame(date = m,rates = survrate)
  surv_rates = rbind(surv_rates,r)
  
  live = month_live_surv(cusdt,orderdt,start,endday)
  r = data.frame(date = m,rates = live)
  mon_live = rbind(mon_live,r)
}

m = as.Date(now())
survrate = survival_rate(cus_flag_func(cusdt,orderdt,m))
survrate = ifelse(length(survrate) ==0,0,survrate)
r = data.frame(date = all_m[length(all_m)],rates = survrate)
surv_rates = rbind(surv_rates,r)

live =  month_live_surv(cusdt,orderdt,as.Date(paste(all_m[length(all_m)],'-01',sep='')),m)
r = data.frame(date = all_m[length(all_m)],rates = live)
mon_live = rbind(mon_live,r)

report_m_1 = cbind(cus_dt_m,order_dt_m[,2],fee_dt_m[,2],surv_rates[,2],mon_live[,2])
colnames(report_m_1) =c('date','cus_num','order_num','total_fee','surv_rate','live_rate')

#日报
days = unique(orderdt$create_date)
artificer_dt = data.frame()
day_info_dt = data.frame()
artificers = unique(na.omit(orderdt$artificer_id))
for(i in 1: length(days))
{
  day = days[i]
  sorders = subset(orderdt,create_date == day)
  day_artificers = na.omit(unique(sorders$artificer_id))
  #每个技师的情况
  for(artificer in day_artificers)
  {
    artificer_orders = subset(sorders,artificer_id == artificer)
    fee = sum(artificer_orders$price,na.rm=T)/100
    num_order = nrow(artificer_orders)
    cus = subset(cusdt,cusdt$id %in% na.omit(artificer_orders$customer_id) & cusdt$create_date >= day)
    ncus_num = nrow(cus)
    r = data.frame(date=day,artificer_id=artificer,order_num=num_order,new_cus=ncus_num,total_fee=fee)
    artificer_dt = rbind(artificer_dt,r)
  }
  
  #每天情况
  cus = subset(cusdt,cusdt$id %in% na.omit(sorders$customer_id) & cusdt$create_date >= day)
  order_num = nrow(sorders)
  cus_num = nrow(cus)
  fee = sum(sorders$price,na.rm=T) / 100
  artificer_num = length(unique(sorders$artificer_id))
  r1 = data.frame(date=day,artificer_num=artificer_num,order_num=order_num,new_cus=cus_num,total_fee=fee)
  day_info_dt = rbind(day_info_dt,r1)
}


conn_td_report <- dbConnect(MySQL(), dbname = "t_d_report", username="wangchao", password="juma9156",host="10.101.0.13",port=3306)
dbWriteTable(conn_td_report, "summary_m", report_m_1,overwrite = T,row.names=F,field.types = list(date='varchar(10)',cus_num='numeric',order_num='numeric',total_fee='decimal(12,5)',surv_rate='decimal(10,5)',live_rate='decimal(10,5)'))
dbWriteTable(conn_td_report, "summary_d_info", day_info_dt,overwrite = T,row.names=F,field.types = list(date='Date',artificer_num='numeric',order_num='numeric',new_cus='numeric',total_fee='decimal(12,5)'))
dbWriteTable(conn_td_report, "summary_d_artificer", artificer_dt,overwrite = T,row.names=F,field.types = list(date='Date',artificer_id='numeric',order_num='numeric',new_cus='numeric',total_fee='decimal(12,5)'))

dbDisconnect(conn_td_report)
