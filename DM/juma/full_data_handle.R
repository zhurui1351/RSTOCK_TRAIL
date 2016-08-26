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

sql = 'SELECT d.create_time,service_date,customer_id,e.name AS customer_name,e.phone, price as amount,pre_price as price,artificer_id,c.name AS artificer_name,c.plate_number, problem_mark,status,channel_id,order_service_str,order_type_id  FROM t_d.order_info d
LEFT JOIN (
SELECT
a.id,NAME,
plate_number
FROM artificer a LEFT JOIN  vehicle_td b ON a.vehicle_id = b.id
) c
ON d.artificer_id = c.id
LEFT JOIN customer e
ON d.customer_id = e.id
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

sub_dt = subset(orderdt,status !='8')
order_date = sub_dt$create_time
order_date_m = strftime(order_date,format = '%Y-%m')
order_dt_m = aggregate(order_date_m,by = list(order_date_m),length)

fee_dt_m = aggregate(as.numeric(sub_dt$price)/ 100,by = list(order_date_m),function(x){sum(x,na.rm=T)})

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
surv_rates = surv_rates[order(surv_rates$date,decreasing=T),]

live =  month_live_surv(cusdt,orderdt,as.Date(paste(all_m[length(all_m)],'-01',sep='')),m)
r = data.frame(date = all_m[length(all_m)],rates = live)
mon_live = rbind(mon_live,r)
mon_live = mon_live[order(mon_live$date,decreasing=T),]

report_m_1 = cbind(cus_dt_m,order_dt_m[,2],fee_dt_m[,2],surv_rates[,2],mon_live[,2])
colnames(report_m_1) =c('date','cus_num','order_num','total_fee','surv_rate','live_rate')

#日报
days = unique(orderdt$create_date)
platenumber_dt = data.frame()
day_info_dt = data.frame()
platenumber = unique(na.omit(orderdt$plate_number))
platenumber = platenumber[which(substr(platenumber,1,1)!='测')]
for(i in 1: length(days))
{
  day = days[i]
  sorders = subset(orderdt,create_date == day  )
  day_platenumber = na.omit(unique(sorders$plate_number))
  #每个技师的情况
  for(plate in day_platenumber)
  {
    plate_orders = subset(sorders,plate_number == plate & status != 8 )
    plate_orders_cancle = subset(sorders,plate_number == plate & status == 8 )
    plate_orders_cancle_num = nrow(plate_orders_cancle)
    fee = sum(plate_orders$price,na.rm=T)/100
    reduce_fee = fee - sum(plate_orders$amount,na.rm=T)/100
    artificer_name = unique(plate_orders$artificer_name)[1]
    ##客户统计
    #拉新
    new_cus = subset(cusdt,cusdt$id %in% na.omit(plate_orders$customer_id) & cusdt$create_date >= day)
    ncus_num = nrow(new_cus)
    
    #绑定微信
    cus_weixin_num = nrow(subset(new_cus,!is.na(wx_openid)))
    #不同用户
    all_cus_day = unique(plate_orders$customer_id)
    #老用户
    old_cus = setdiff(all_cus_day,new_cus$id)
    ocus_num = length(old_cus)
    #激活用户
    acus_num = 0
    for(cu in all_cus_day)
    {
      cu_info = subset(cusdt,cusdt$id == cu)
      cu_order_pre = subset(orderdt,customer_id==cu & create_date<day)
      if(cu_info$create_date < day && nrow(cu_order_pre) == 1)
      {
        acus_num = acus_num + 1
      }
    }
    #订单统计
    #老用户下单
    old_cus_order = subset(plate_orders,customer_id %in% old_cus)
    old_cus_order_num = nrow(old_cus_order)
    #新用户下单
    new_cus_order = subset(plate_orders,customer_id %in% new_cus)
    new_cus_order_num = nrow(new_cus_order)
    #当天新订单
    num_order = nrow(plate_orders)
    #历史未处理订单
    history_order = subset(orderdt,plate_number == plate & create_date <= day & status != 8 & !(status %in% c(6,7)))
    history_order_num = nrow(history_order)
    #接单数
    accpt_order = subset(plate_orders,status >=3)
    accpt_order_num = nrow(accpt_order)
    #发起结算
    start_pay_order = subset(plate_orders,status >=4)
    start_pay_order_num = nrow(start_pay_order)
    #已支付
    payed_order = subset(plate_orders,status >=6)
    payed_order_num = nrow(payed_order)
    #待接单
    wait_accpt_order =  subset(plate_orders,status == 2)
    wait_accpt_order_num = nrow(wait_accpt_order)
    #待付款
    wait_pay_order =  subset(plate_orders,status == 4)
    wait_pay_order_num = nrow(wait_pay_order)
    #待确认
    wait_sure_order =  subset(plate_orders,status == 5)
    wait_sure_order_num = nrow(wait_sure_order)
    #接单率
    accpt_ratio = accpt_order_num / num_order
    #订单完成率
    complete_ratio = payed_order_num / num_order
    #待结算订单率
    wait_pay_ratio = wait_pay_order_num / num_order
    #取消率
    cacel_ratio = plate_orders_cancle_num /  num_order
    
    #线上单
    online_order = subset(plate_orders,channel_id ==1)
    online_order_num = nrow(online_order)
    #客服单
    cuserivice_order = subset(plate_orders,channel_id ==2)
    cuserivice_order_num = nrow(cuserivice_order)
    
   # r = data.frame(date=day,plate_number=plate,order_num=num_order,new_cus=ncus_num,total_fee=fee)
    r = data.frame(date=day,plate_number=plate,artificer_name=artificer_name,history_order_num=history_order_num,order_num=num_order,new_cus=ncus_num,cus_weixin_num=cus_weixin_num,
                   active_cus_num=acus_num,new_cus_order_num=new_cus_order_num,old_cus_order_num=old_cus_order_num,accpt_order_num=accpt_order_num,start_pay_order_num=start_pay_order_num,
                   payed_order_num=payed_order_num,online_order_num=online_order_num,cuserivice_order_num=cuserivice_order_num,plate_orders_cancle_num=plate_orders_cancle_num,total_fee=fee,reduce_fee,
                   wait_accpt_order=wait_accpt_order_num,wait_pay_order=wait_pay_order_num,accpt_ratio=accpt_ratio,complete_ratio=complete_ratio,cacel_ratio)
    platenumber_dt = rbind(platenumber_dt,r)
  }
  
  #每天情况
  day_orders = subset(sorders,status != 8)
  #客户
  all_day_cus = unique(na.omit(day_orders$customer_id))
  #拉新
  new_cus = subset(cusdt,cusdt$id %in% all_day_cus & cusdt$create_date == day)
  new_cus_num = nrow(new_cus)
  #历史未处理订单
  history_order = subset(orderdt,create_date <= day & status != 8 & !(status %in% c(6,7)))
  history_order_num = nrow(history_order)
  #老用户
  old_cus = setdiff(all_cus_day,new_cus$id)
  ocus_num = length(old_cus)
  #激活用户
  acus_num = 0
  for(cu in all_cus_day)
  {
    cu_info = subset(cusdt,cusdt$id == cu)
    cu_order_pre = subset(orderdt,customer_id==cu & create_date<day)
    if(cu_info$create_date < day && nrow(cu_order_pre) == 1)
    {
      acus_num = acus_num + 1
    }
  }
  #订单统计
  #老用户下单
  old_cus_order = subset(day_orders,customer_id %in% old_cus)
  old_cus_order_num = nrow(old_cus_order)
  #新用户下单
  new_cus_order = subset(day_orders,customer_id %in% new_cus)
  new_cus_order_num = nrow(new_cus_order)
  
  #绑定微信
  cus_weixin_num = nrow(subset(new_cus,!is.na(wx_openid)))
  #订单
  order_num = nrow(day_orders)
  #线上单
  online_order_num = nrow(subset(day_orders,channel_id ==1))
  #客服单
  cuserivice_order_num = nrow(subset(day_orders,channel_id ==2))
  #接单数
  accpt_order = subset(day_orders,status >=3)
  accpt_order_num = nrow(accpt_order)
  #发起结算
  start_pay_order = subset(day_orders,status >=4)
  start_pay_order_num = nrow(start_pay_order)
  #活动单
  activiti_order =  subset(day_orders,order_type_id ==3)
  activiti_order_num = nrow(activiti_order)
  #维修保养
  order_service_str = day_orders$order_service_str
  repair_order = grep('.*[78].*',order_service_str)
  repair_order_num = length(repair_order)
  preserv_order = grep('.*[36].*',order_service_str)
  preserv_order_num =length(preserv_order)
  #已支付
  payed_order = subset(day_orders,status >=6)
  payed_order_num = nrow(day_orders)
  #已回访
  reaccess_order =  subset(day_orders,status ==7)
  reaccess_order_num = nrow(reaccess_order)
  #取消
  cancle_order =  subset(day_orders,status ==8)
  cancle_order_num = nrow(cancle_order)
  
  #待派单
  wait_distribute_order = subset(day_orders,status ==1)
  wait_distribute_order_num = nrow(wait_distribute_order)
  #待接单
  wait_accpt_order =  subset(day_orders,status ==2)
  wait_accpt_order_num = nrow(wait_accpt_order)
  #待付款
  wait_pay_order =  subset(day_orders,status == 4)
  wait_pay_order_num = nrow(wait_pay_order)
  #待确认
  wait_sure_order =  subset(day_orders,status == 5)
  wait_sure_order_num = nrow(wait_sure_order)
  #待回访
  wait_reaccess_order = subset(day_orders,status ==6)
  wait_reaccess_order_num = nrow(wait_reaccess_order)
  #接单率
  accpt_ratio = accpt_order_num / order_num
  #订单完成率、付款率
  complete_ratio = payed_order_num / order_num
  #待结算订单率
  wait_pay_ratio = wait_pay_order_num / order_num
  #取消率
  cacel_ratio = cancle_order_num /  order_num
  #派单率
  distribute_ratio = (order_num - wait_distribute_order_num) / order_num
  #回访率
  reaccess_ratio = reaccess_order_num / order_num
  
  fee = sum(day_orders$price,na.rm=T) / 100
  reduce_fee = fee - sum(day_orders$amount,na.rm=T)/100
  
  repair_fee = sum(day_orders[repair_order,]$price,na.rm = T) / 100
  preserv_fee = sum(day_orders[preserv_order,]$price,na.rm = T) / 100
    
  plate_num = length(unique(day_orders$plate_number))
  r1 = data.frame(date=day,plate_num=plate_num,history_order_num=history_order_num,order_num=order_num,cuserivice_order_num=cuserivice_order_num,online_order_num=online_order_num,
                  activiti_order_num=activiti_order_num,new_cus=new_cus_num,num_weixin = cus_weixin_num,acus_num=acus_num,new_cus_order_num=new_cus_order_num,
                  old_cus_order_num=old_cus_order_num,new_cus_order_num=new_cus_order_num,accpt_order_num=accpt_order_num,start_pay_order_num=start_pay_order_num,
                  repair_order_num=repair_order_num,preserv_order_num=preserv_order_num,payed_order_num=payed_order_num,reaccess_order_num=reaccess_order_num,
                  cancle_order_num=cancle_order_num,total_fee=fee,repair_fee=repair_fee,preserv_fee=preserv_fee,reduce_fee=reduce_fee,wait_distribute_order_num=wait_distribute_order_num,
                  wait_accpt_order_num=wait_accpt_order_num,wait_pay_order_num=wait_pay_order_num,wait_sure_order_num=wait_sure_order_num,accpt_ratio=accpt_ratio,wait_pay_ratio=wait_pay_ratio,
                  cacel_ratio=cacel_ratio,distribute_ratio=distribute_ratio,reaccess_ratio=reaccess_ratio)
  day_info_dt = rbind(day_info_dt,r1)
  
  
  #每天客户情况
  cusflag = cus_flag_func(cusdt,orderdt,day)
  cus_day = subset(cusdt,create_date <= day)
  cus_num = nrow(cus_day)
  new_cus = subset(cusdt,create_date == day)
  new_cus_num = nrow(new_cus)
  #微信绑定
  cus_weixin_num = nrow(subset(cus_day,!is.na(wx_openid)))
  #留存用户
  live_cus_num = nrow(subset(cusflag,flag == '留存用户'))
  #流失用户
  death_cus_num = nrow(subset(cusflag,flag == '流失用户'))
  
}

#n天以上未访问客户

cusinfo_dt = merge(cusdt,cusflag,by.x = 'id',by.y = 'cusno',all.x = T)

#技师维度

sorderdt = orderdt[,c('create_time','customer_id','customer_name','phone','price','artificer_id','artificer_name','plate_number','problem_mark','create_date')]

#写入数据库
conn_td_report <- dbConnect(MySQL(), dbname =dbname_td_report, username=username, password=password,host=host,port=port)
dbSendQuery(conn_td_report,'SET NAMES gbk')
dbWriteTable(conn_td_report, "summary_m", report_m_1,overwrite = T,row.names=F,field.types = list(date='varchar(10)',cus_num='numeric',order_num='numeric',total_fee='decimal(12,5)',surv_rate='decimal(10,5)',live_rate='decimal(10,5)'))

dbWriteTable(conn_td_report, "summary_d_info", day_info_dt,overwrite = T,row.names=F,field.types = list(date='Date',plate_num='numeric',order_num='numeric',new_cus='numeric',num_weixin='numeric',total_fee='decimal(12,5)'))

dbWriteTable(conn_td_report, "summary_d_plate", platenumber_dt,overwrite = T,row.names=F,field.types = list(date='Date',plate_number='varchar(20)',artificer_name='varchar(20)',history_order_num='numeric',order_num='numeric',new_cus='numeric',
                                                                                                            cus_weixin_num='numeric',active_cus_num='numeric',new_cus_order_num='numeric',old_cus_order_num='numeric',accpt_order_num='numeric',
                                                                                                            start_pay_order_num='numeric',payed_order_num='numeric',online_order_num='numeric',cuserivice_order_num='numeric',
                                                                                                            plate_orders_cancle_num='numeric',total_fee='decimal(12,5)',reduce_fee='decimal(12,5)',wait_accpt_order='numeric',wait_pay_order='numeric',
                                                                                                            accpt_ratio='decimal(12,5)',complete_ratio='decimal(12,5)',cacel_ratio='decimal(12,5)'))

dbWriteTable(conn_td_report, "summary_d_customer_info", cusinfo_dt,overwrite = T,row.names=F,field.types = list(id='numeric',name='varchar(20)',phone='varchar(20)',create_time='datetime',wx_openid='varchar(100)',plate_number='varchar(20)',create_date ='Date',flag='varchar(255)',lastdate='Date'))

dbWriteTable(conn_td_report, "summary_d_order_info", sorderdt,overwrite = T,row.names=F,field.types = list(create_time='datetime',customer_id='numeric',customer_name='varchar(20)',phone='varchar(20)',price='numeric',artificer_id='numeric',artificer_name='varchar(20)',plate_number='varchar(20)',problem_mark='varchar(255)',create_date ='Date'))

dbDisconnect(conn_td_report)



