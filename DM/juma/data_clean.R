rm(list = ls(all=T))
require(RMySQL)
require(lubridate)
source('D:/Rcode/code/RSTOCK_TRAIL/DM/juma/car_doc_func.R',encoding = 'utf8')

#全局变量

date = Sys.Date()
date_y = year(date)

#客户表
cusdt = read.csv('d:/jumpdata/cardoc/customer.csv',head=T,skip=1,stringsAsFactors = F,na.strings = c("NA",' ','\\',''),
                 sep=',',strip.white=T,blank.lines.skip=T)

cusdt = subset(cusdt,nchar(客户姓名) > 1 | nchar(车牌号码) > 1)

cusdt$首次服务日期 =  gsub('[年月]','-',cusdt$首次服务日期)
cusdt$首次服务日期 =  gsub('日','',cusdt$首次服务日期)

#订单表
orderdt_2016 = read.csv('d:/jumpdata/cardoc/order_2016.csv',head=T,skip=1,stringsAsFactors = F,na.strings = c("NA",' ','\\',''),
                   sep=',',strip.white=T,blank.lines.skip=T)

orderdt_2015 = read.csv('d:/jumpdata/cardoc/order_2015.csv',head=T,skip=1,stringsAsFactors = F,na.strings = c("NA",' ','\\',''),
                        sep=',',strip.white=T,blank.lines.skip=T)

#完善日期
orderdt_2016$服务日期 = paste(date_y,'年',orderdt_2016$服务日期,sep='')
orderdt_2015$服务日期 = paste('2015','年',orderdt_2015$服务日期,sep='')
colnames(orderdt_2015) = colnames(orderdt_2016)

orderdt = rbind(orderdt_2015,orderdt_2016)

#分隔服务时间
tm = strsplit(orderdt$服务时间,"-")
starttm = sapply(tm,function(x) x[1])
endtm = sapply(tm,function(x) x[2])

orderdt$starttm = starttm
orderdt$endtm = endtm

orderdt$服务日期 =  gsub('[年月]','-',orderdt$服务日期)
orderdt$服务日期 =  gsub('日','',orderdt$服务日期)

orderdt$starttm = ifelse(is.na(orderdt$starttm),NA,paste(orderdt$服务日期,orderdt$starttm,sep = ' '))
orderdt$endtm = ifelse(is.na(orderdt$endtm),NA,paste(orderdt$服务日期,orderdt$endtm,sep = ' '))



cusnos = c()
#关联
for(i in 1:nrow(orderdt))
{
  r = orderdt[i,]
  #按车牌进行匹配
  if(!is.na(r[1,'车牌号']) && !is.null(r[1,'车牌号']) && nchar(r[1,'车牌号']) > 1)
  {
    subdt = subset(cusdt,车牌号码== r[1,'车牌号'] )[1,]
    if(!is.null(subdt) && nrow(subdt) > 0 && !is.na(subdt[,'序号']) && !is.null(subdt[,'序号']) )
    {
      cusno = subdt[,'序号']
      cusnos = c(cusnos,cusno)
      next
    }

  }
  #按车牌无法索引到客户
  if(!is.na(r[1,'联系方式']) && !is.null(r[1,'联系方式']) && nchar(r[1,'联系方式']) > 1)
  {
    subdt = subset(cusdt,电话== r[1,'联系方式'] )[1,]
    if( nrow(subdt) > 0 && !is.na(subdt[,'序号']) && !is.null(subdt[,'序号']) )
    {
      cusno = subdt[,'序号']
      cusnos = c(cusnos,cusno)
      next
    }
  }
  #按客户姓名
  if(!is.na(r[1,'客户姓名']) && !is.null(r[1,'客户姓名']) && nchar(r[1,'客户姓名']) > 1)
  {
    subdt = subset(cusdt,客户姓名== r[1,'客户姓名'] )[1,]
    if( nrow(subdt) > 0 && !is.na(subdt[,'序号']) && !is.null(subdt[,'序号']) )
    {
      cusno = subdt[,'序号']
      cusnos = c(cusnos,cusno)
      next
    }
  }
 
  cusnos = c(cusnos,NA)
    
}

orderdt$cusno = cusnos

orderdt$序号 = paste(orderdt$服务日期,orderdt$序号,sep='')
orderdt$序号 = gsub('-','',orderdt$序号)

#处理客户首次服务时间

for(i in 1 : nrow(cusdt))
{
  r = cusdt[i,]
  cus_no = r[,'序号']
  orders = subset(orderdt,cusno == cus_no)
  if(nrow(orders) == 0 || is.na(orders[1,'序号']) || is.null(orders[1,'序号']))
  {
    next
  }
  
  tms = orders[,'服务日期']
  tm = tms[order(as.Date(tms),decreasing = F,na.last = T)][1]
  cusdt[i,'首次服务日期'] = tm
}

cusdt[,'首次服务日期'] = as.Date(cusdt[,'首次服务日期'])
orderdt$收费合计 = as.numeric(orderdt$收费合计)
cusdt$首次服务日期 = as.Date(cusdt$首次服务日期)
orderdt$服务日期 = as.Date(orderdt$服务日期)

cus = getCustomerfromOrder_all(data.frame(),orderdt)
  
  
savepath = 'D:/jumpdata/cardoc/'
write.csv(orderdt,file = paste(savepath,'订单表汇总.csv',sep=''),row.names = F)
write.csv(cusdt,file = paste(savepath,'客户表汇总.csv',sep=''),row.names = F)

#conn <- dbConnect(MySQL(), dbname = "report", username="report", password="report",host="10.101.0.102",port=3306)
#dbSendQuery(conn,'SET NAMES gbk')
#dbReadTable(conn, "CUSTOMER_INFO") 
#dbDisconnect(conn)

#微信id
weixin_path = 'D:/jumpdata/cardoc/sqlresult.csv'
weixindt = read.csv(weixin_path,head=T,stringsAsFactors = F,na.strings = c("NA",' ','\\',''),
                 sep=',',strip.white=T,blank.lines.skip=T)
weixindt$Bind_time = as.POSIXct(weixindt$Bind_time,format = '%Y/%m/%d %H:%S',tz='')
weixin_info = data.frame()
for(i in 1:nrow(orderdt))
{
  order = orderdt[i,]
  swei = subset(weixindt,weixindt$User_phone == order$联系方式)
  if(nrow(swei) >= 1)
  {
    r = data.frame(bind_time = swei$Bind_time,wxid = swei$User_wxid)
  }
  else
  {
    r = data.frame(bind_time = NA,wxid = NA)
  }
  weixin_info = rbind(weixin_info,r)
}
orderdt = cbind(orderdt,weixin_info)
write.csv(orderdt,file = paste(savepath,'订单表汇总.csv',sep=''),row.names = F)


####
ttm = c()
for(i in 1 : length(starttm))
{
  x = starttm[i]
  x = gsub(' ','',x)
  x = gsub('：',':',x)
  
  if(is.null(x) || is.na(x) || x == '/')
  {
   ttm = c(ttm,'00:00:01')
  }
  else
  {
    ttm = c(ttm,paste(x,':01',sep=''))
  }
}
ttm1 = paste(orderdt$服务日期,ttm,sep = ' ')
orderdt$服务时间 = ttm1

write.csv(orderdt,file = paste(savepath,'订单表汇总.csv',sep=''),row.names = F)
