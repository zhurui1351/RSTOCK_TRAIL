require(RMySQL)
require(lubridate)
#全局变量

date = Sys.Date()
date_y = year(date)

#客户表
cusdt = read.csv('d:/jumpdata/cardoc/customer.csv',head=T,skip=1,stringsAsFactors = F,na.strings = c("NA",' ','\\'),
                 sep=',',strip.white=T,blank.lines.skip=T)

cusdt = subset(cusdt,nchar(客户姓名) > 1 | nchar(车牌号码) > 1)

cusdt$首次服务日期 =  gsub('[年月]','-',cusdt$首次服务日期)
cusdt$首次服务日期 =  gsub('日','',cusdt$首次服务日期)

#订单表
orderdt = read.csv('d:/jumpdata/cardoc/order.csv',head=T,skip=1,stringsAsFactors = F,na.strings = c("NA",' ','\\'),
                   sep=',',strip.white=T,blank.lines.skip=T)

cusnos = c()
#关联
for(i in 1:nrow(orderdt))
{
  r = orderdt[i,]
  #按车牌进行匹配
  if(!is.na(r[1,'车牌号']) && !is.null(r[1,'车牌号']) && nchar(r[1,'车牌号']) > 1)
  {
    subdt = subset(cusdt,车牌号码== r[1,'车牌号'] )[1,]
    if(nrow(subdt) > 0 && !is.na(subdt[,'序号']) && !is.null(subdt[,'序号']) )
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
    if(nrow(subdt) > 0 && !is.na(subdt[,'序号']) && !is.null(subdt[,'序号']) )
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
    if(nrow(subdt) > 0 && !is.na(subdt[,'序号']) && !is.null(subdt[,'序号']) )
    {
      cusno = subdt[,'序号']
      cusnos = c(cusnos,cusno)
      next
    }
  }
 
  cusnos = c(cusnos,NA)
    
}

orderdt$cusno = cusnos
#完善日期
orderdt$服务日期 = paste(date_y,'年',orderdt$服务日期,sep='')
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


#处理客户首次服务时间


#conn <- dbConnect(MySQL(), dbname = "report", username="report", password="report",host="10.101.0.102",port=3306)
#dbSendQuery(conn,'SET NAMES gbk')
#dbReadTable(conn, "CUSTOMER_INFO") 
#dbDisconnect(conn)
