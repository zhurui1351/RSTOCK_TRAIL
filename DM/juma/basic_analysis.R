
kpi_date = as.Date('2016-06-01')

increase_cus =cusdt[which(cusdt$首次服务日期 >= kpi_date),]
increase_cus_num = nrow(increase_cus)


increase_orders =orderdt[which(orderdt$服务日期 >= kpi_date),]
increase_orders_num = nrow(increase_orders)

suborders = orderdt[which(orderdt$服务日期 >= kpi_date),]

aggregate(suborders$服务车编号,by=list(suborders$服务车编号),length)

address = orderdt$服务地址
