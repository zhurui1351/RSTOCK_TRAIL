
kpi_date = as.Date('2016-06-01')

#新增用户，服务单情况
increase_cus =cusdt[which(cusdt$首次服务日期 >= kpi_date),]
increase_cus_num = nrow(increase_cus)


increase_orders =orderdt[which(orderdt$服务日期 >= kpi_date),]
increase_orders_num = nrow(increase_orders)

suborders = orderdt[which(orderdt$服务日期 >= kpi_date),]

#kpi 趋势
#客户
cus_date = cusdt$首次服务日期
cus_date_m = strftime(cus_date,format = '%Y-%m')
cus_dt_m = aggregate(cus_date_m,by = list(cus_date_m),length)



cus_date_w = strftime(cus_date,format = '%Y-%W')
cus_dt_w = aggregate(cus_date_w,by = list(cus_date_w),length)
#订单
order_date = orderdt$服务日期
order_date = as.Date(order_date)
order_date_m = strftime(order_date,format = '%Y-%m')
order_dt_m = aggregate(order_date_m,by = list(order_date_m),length)

order_date_w = strftime(order_date,format = '%Y-%W')
order_dt_w = aggregate(order_date_w,by = list(order_date_w),length)

plot(order_dt_m[,2],xaxt = 'n',type='o',col='red',xlab='',ylab = '')
text(x=1:length(order_dt_m[,2]),y=order_dt_m[,2] + 20,labels=order_dt_m[,2],col = 'red')
axis(1, 1:nrow(cus_dt_m),cus_dt_m[,1])
lines(cus_dt_m[,2],type="o",pch=22,lty=2,col='green')
text(x=1:length(cus_dt_m[,2]),y=cus_dt_m[,2],labels=cus_dt_m[,2],col='green')

title(main="增长趋势", col.main="red", font.main=4)
title(xlab= "日期", col.lab=rgb(0,0.5,0))
title(ylab= "数量", col.lab=rgb(0,0.5,0))
legend(1, max(order_dt_m[,2]), c('订单增长','客户增长'), cex=0.8, col=c('red','green'), pch=21:22, lty=1:2);
#服务车辆车次的分布
cars_num = data.frame()
cars = unique(suborders$服务车编号)
for(car in cars)
{
  dates = subset(suborders,服务车编号 == car)[,'服务日期']
  dates_num = length(unique(dates))
  r = data.frame(车牌=car,出车=dates_num)
  cars_num = rbind(cars_num,r)
}
cars_num
#分布地域

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


#活跃度，总体分析
enddate = as.Date('2016-06-14')
startdate = enddate - 60

subcusnos = na.omit(subset(cusdt,as.Date(首次服务日期) > startdate)$序号) 

subcusnos = na.omit(cusdt$序号) 


suborderdt = subset(orderdt,cusno %in% subcusnos)

dt = aggregate(suborderdt[,'cusno'],by=list(suborderdt[,'cusno']),length)
dt_sum = aggregate(dt[,2],by=list(dt[,2]),length)
colnames(dt_sum) = c('使用次数','车辆总数')

subset(dt,dt[,2] == 10)

#活跃度细分
#未满60天
#45天以内，体验客户
cusflag = data.frame()
cusnos = na.omit(cusdt$序号)
for(no in cusnos)
{
  cusorders = subset(orderdt,orderdt$cusno == no)
  if(nrow(cusorders) == 0) next
  
  dates = as.Date(cusorders$服务日期[order(as.Date(cusorders$服务日期),decreasing = F)]) 
  firstdate = dates[1]
  lastdate = dates[length(dates)]
  num = length(dates)
  
  #首次使用45天以内，只使用过一次
  if(num == 1 && firstdate >= (enddate - 45) )
  {
    flag = '体验用户'
  }
  #只使用过一次，首次使用在46-60天以内
  else if(num == 1 && (firstdate >= (enddate - 60)) && (firstdate < (enddate - 45)))
  {
    flag = '危险体验用户'
  }
  #使用两次以上,首次在60天以内最近一次在45天以内
  else if(num >= 2 && firstdate >= (enddate - 60) && lastdate >= (enddate - 45))
  {
    flag = '激活用户'
  }
  #使用两次以上，首次使用60天以内，最近一次在45天到60天
  else if(num >= 2 && (firstdate >= (enddate - 60)) && (lastdate < (enddate - 45)) && (lastdate >= (enddate - 60)) )
  {
    flag = '危险激活用户'
  }
  #只使用过一次，首次时间在60天以上
  else if(num == 1 &&  firstdate < (enddate - 60) )
  {
    flag = '流失体验用户'
  }
  #首次时间60天以上，两次以上，最近一次在45天以内
  else if(num >= 2 && firstdate < (enddate - 60)  && (lastdate >= (enddate - 45)) )
  {
    flag = '留存用户'
  }
  #首次使用60天以上，两次以上，最近一次在46 - 60天
  else if(num >= 2 && firstdate < (enddate - 60)  && (lastdate < (enddate - 45))  && (lastdate >= (enddate - 60)) )
  {
    flag = '危险用户'
  }
  #首次使用在60天以上，两次以上，最近一次60天以上
  else if(num >= 2 && firstdate < (enddate - 60)  && (lastdate < (enddate - 60)) )
  {
    flag = '流失用户'
  }
  else
  {
    flag = NA
  }
   r= data.frame(cusno = no,flag = flag)
   cusflag = rbind(cusflag,r)
}

dt_cus_flag = aggregate(cusflag$flag,by = list(cusflag$flag),length)
colnames(dt_cus_flag) = c('客户类比','客户量')
dt_cus_flag$比例 = dt_cus_flag$客户量/nrow(cusflag)

#活动分析
#备注及服务项目中出现氟
activitidate = as.Date('2016-06-13')
s1 = grep('氟',orderdt$服务项目)
s2 = grep('氟',orderdt$备注)
s = union(s1,s2)

s = orderdt[s,]
actorders = subset(s,s$服务日期 >= activitidate)
actcus = unique(na.omit(actorders$cusno))

preorders = subset(orderdt,orderdt$服务日期 < activitidate)

#老客户激活
oldcus = subset(cusdt,as.Date(cusdt$首次服务日期) < activitidate )$序号
reaccesscus = actcus[actcus %in% oldcus]

preoldolders = subset(preorders,cusno %in% reaccesscus)

predt = aggregate(as.Date(preoldolders[,c('服务日期')]),by = list(preoldolders$cusno),max)
colnames(predt) = c('客户号','最近访问日期')
predt$距离最近未访问天数 = activitidate - predt$最近访问日期

#真实拉新
newcus = actcus[!(actcus %in% oldcus)]

#拉新激活
neworders = subset(actorders,cusno %in% newcus)
aggregate(neworders$cusno,by = list(neworders$cusno),length)

oldorders = subset(actorders,cusno %in% oldcus)
aggregate(oldorders$cusno,by = list(oldorders$cusno),length)
