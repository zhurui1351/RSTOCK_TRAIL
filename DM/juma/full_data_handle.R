rm(list = ls(all=T))
require(RMySQL)
require(lubridate)
source('D:/Rcode/code/RSTOCK_TRAIL/DM/juma/car_fun_online.R',encoding = 'utf8')

dbname_td = "t_d"
dbname_td_report = 't_d_report'
username="wangchao"
password = 'juma9156'
host = '10.101.0.13'
port = 3306

conn_td <- dbConnect(MySQL(), dbname = dbname_td, username=username, password=password,host=host,port=port)
conn_td_report <- dbConnect(MySQL(), dbname =dbname_td_report, username=username, password=password,host=host,port=port)
dbSendQuery(conn_td,'SET NAMES gbk')
dbSendQuery(conn_td_report,'SET NAMES gbk')

sql = 'SELECT d.create_time,customer_id,e.name AS customer_name,e.phone,pre_price as price,artificer_id,c.name AS artificer_name,c.plate_number, problem_mark FROM t_d.order_info d
LEFT JOIN (
SELECT
a.id,NAME,
plate_number
FROM artificer a LEFT JOIN  vehicle_td b ON a.vehicle_id = b.id
) c
ON d.artificer_id = c.id
LEFT JOIN customer e
ON d.customer_id = e.id
WHERE STATUS IN (4,5,6,7)
ORDER BY create_time DESC '

orderdt = dbGetQuery(conn_td,sql)
orderdt$create_date = as.Date(orderdt$create_time)
sql = 'SELECT id ,name,phone,create_time ,wx_openid,plate_number FROM customer  c
LEFT JOIN (
SELECT a.customer_id,GROUP_CONCAT(DISTINCT b.plate_number) AS plate_number  FROM customer_vehicle_relation a
LEFT JOIN customer_vehicle b ON a.customer_vehicle_id = b.id 
GROUP BY a.customer_id
) d ON c.id = d.customer_id
'
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
cusflag = cus_flag_func(cusdt,orderdt,m)
survrate = survival_rate(cusflag)
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
platenumber_dt = data.frame()
day_info_dt = data.frame()
platenumber = unique(na.omit(orderdt$plate_number))
for(i in 1: length(days))
{
  day = days[i]
  sorders = subset(orderdt,create_date == day)
  day_platenumber = na.omit(unique(sorders$plate_number))
  #每个技师的情况
  for(plate in day_platenumber)
  {
    plate_orders = subset(sorders,plate_number == plate)
    fee = sum(plate_orders$price,na.rm=T)/100
    num_order = nrow(plate_orders)
    cus = subset(cusdt,cusdt$id %in% na.omit(plate_orders$customer_id) & cusdt$create_date >= day)
    ncus_num = nrow(cus)
    r = data.frame(date=day,plate_number=plate,order_num=num_order,new_cus=ncus_num,total_fee=fee)
    platenumber_dt = rbind(platenumber_dt,r)
  }
  
  #每天情况
  cus = subset(cusdt,cusdt$id %in% na.omit(sorders$customer_id) & cusdt$create_date == day)
  order_num = nrow(sorders)
  cus_num = nrow(cus)
  cus_weixin_num = nrow(subset(cus,!is.na(wx_openid)))
  fee = sum(sorders$price,na.rm=T) / 100
  plate_num = length(unique(sorders$plate_number))
  r1 = data.frame(date=day,plate_num=plate_num,order_num=order_num,new_cus=cus_num,num_weixin = cus_weixin_num,total_fee=fee)
  day_info_dt = rbind(day_info_dt,r1)
}

#n天以上未访问客户

cusinfo_dt = merge(cusdt,cusflag,by.x = 'id',by.y = 'cusno',all.x = T)

conn_td_report <- dbConnect(MySQL(), dbname =dbname_td_report, username=username, password=password,host=host,port=port)
dbSendQuery(conn_td_report,'SET NAMES gbk')
dbWriteTable(conn_td_report, "summary_m", report_m_1,overwrite = T,row.names=F,field.types = list(date='varchar(10)',cus_num='numeric',order_num='numeric',total_fee='decimal(12,5)',surv_rate='decimal(10,5)',live_rate='decimal(10,5)'))
dbWriteTable(conn_td_report, "summary_d_info", day_info_dt,overwrite = T,row.names=F,field.types = list(date='Date',plate_num='numeric',order_num='numeric',new_cus='numeric',num_weixin='numeric',total_fee='decimal(12,5)'))
dbWriteTable(conn_td_report, "summary_d_plate", platenumber_dt,overwrite = T,row.names=F,field.types = list(date='Date',plate_number='varchar(20)',order_num='numeric',new_cus='numeric',total_fee='decimal(12,5)'))
dbWriteTable(conn_td_report, "summary_d_customer_info", cusinfo_dt,overwrite = T,row.names=F,field.types = list(id='numeric',name='varchar(20)',phone='varchar(20)',create_time='datetime',wx_openid='varchar(100)',plate_number='varchar(20)',create_date ='Date',flag='varchar(255)',lastdate='Date'))
dbWriteTable(conn_td_report, "summary_d_order_info", orderdt,overwrite = T,row.names=F,field.types = list(create_time='datetime',customer_id='numeric',customer_name='varchar(20)',phone='varchar(20)',price='numeric',artificer_id='numeric',artificer_name='varchar(20)',plate_number='varchar(20)',problem_mark='varchar(255)',create_date ='Date'))

dbDisconnect(conn_td_report)

