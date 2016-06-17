
kpi_date = as.Date('2016-06-01')

increase_cus =cusdt[which(cusdt$首次服务日期 >= kpi_date),]
increase_cus_num = nrow(increase_cus)


increase_orders =orderdt[which(orderdt$服务日期 >= kpi_date),]
increase_orders_num = nrow(increase_orders)

suborders = orderdt[which(orderdt$服务日期 >= kpi_date),]

cars_num = data.frame()
cars = unique(suborders$服务车编号)
for(car in cars)
{
  dates = subset(suborders,服务车编号 == car)[,'服务日期']
  dates_num = length(unique(dates))
  r = data.frame(车牌=car,出车=dates_num)
  cars_num = rbind(cars_num,r)
}

address = orderdt$服务地址
aggregate(address,by=list(address),length)

keywords = c('金府','安靖','八里','八一|八益','白家','百脑汇','北富森','博雅','成绵','川陕','福锦路|聚龙|巨龙',
             '剑龙','金府','万贯','量力','龙泉','龙潭','摩尔','郫县','十陵','双流','新都','金三角')

flagwords = c()
mathed = F
for(a in address)
{
  for(k in keywords)
  {
    i = grep(k,a)
    if(length(i) > 0)
    {
      flagwords = c(flagwords,k)
      mathed = T
      break
    }
  }
  if(!mathed)
  {
    flagwords = c(flagwords,a)
  }
  else
  {
    mathed = F
  }
}
aggregate(flagwords,by=list(flagwords),length)


#活跃度
enddate = as.Date('2016-06-14')
startdate = enddate - 60


