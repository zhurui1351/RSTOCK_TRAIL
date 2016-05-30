require('data.table')
#tb=fread('1.txt',sep=',') 
#df = as.data.frame((tb))
#hive -e to export data
tb=fread('1.txt',sep='\t',colClasses=c(rep('numeric',3),rep('character',2),rep('numeric',3),rep('character',5),'numeric',rep('character',2)))
x=head(tb,1)
class(x)
head(tb[,(V4)])
col=c(rep('character',2),rep('numeric',1),rep('character',3),rep('numeric',1),rep('character',3),
      rep('numeric',34),rep('character',7),rep('numeric',1),rep('character',2),rep('numeric',1),
      rep('character',2),rep('numeric',17),rep('character',3),rep('numeric',1),rep('character',1))

col = c(rep('character',4),rep('numeric',2),rep('character',3))
tb=fread('/rstudio/tmpsn.txt',sep='\t',colClasses = col,nrow=10)

#hive -e 'select cert_no,age,sex,phone_city,user_level,buy_m,yr_order_cnt,yr_order_amt,yr_max_amt,yr_order_amt_rank,yr_avgmonth_cnt,mth_order_cnt6,mth_order_amt6,mth_order_cnt3,mth_order_amt3,yr_maxmonth_amt,city_ofen,bankcard_cnt,cdt_card_cnt,onli_pay_cnt,onli_pay_amt,offli_pay_cnt,offli_pay_amt from base_ads.cert_acct_sum' > /rstudio/zhurui/cert_acct_sum.txt
tb=fread('/rstudio/zhurui/cert_acct_sum.txt',sep='\t',nrow=10000,verbose = T,na.strings=c("NA","NULL"))

col = sapply(tb, class)
tb=fread('/rstudio/zhurui/cert_acct_sum.txt',sep='\t',nrow=53551637,colClasses = col,verbose = T,na.strings=c("NA","NULL")
         ,col.names=c('cert_no','age','sex','phone_city','user_level','buy_m','yr_order_cnt','yr_order_amt','yr_max_amt','yr_order_amt_rank','yr_avgmonth_cnt','mth_order_cnt6','mth_order_amt6','mth_order_cnt3','mth_order_amt3','yr_maxmonth_amt','city_ofen','bankcard_cnt','cdt_card_cnt','onli_pay_cnt','onli_pay_amt','offli_pay_cnt','offli_pay_amt'))


tb=fread('/rstudio/zhurui/leijixiaofei.txt',sep='\t',nrow=10000,verbose = T,header=T,na.strings=c("NA","NULL"))

col = sapply(tb, class)
tb=fread('/rstudio/zhurui/leijixiaofei.txt',sep='\t',verbose = T,colClasses=col,header=T,na.strings=c("NA","NULL"),encoding="UTF-8")
colnames(tb) = gsub('leijixiaofei.','',colnames(tb),fixed=T)

acct =c('gonglinan_mpx','cyh836206','lo5u','zhenghaor10','majiao0454','carrot_mount','jertormy','jd_5c24cde9be66e','13551364748_p','tanghongshan','开心购-0516','beijingyidilei','翟欢','fensionenergy','再次28981723','osfox','junfu83','138111838886_p','zhgning168','caonannan','crniuniu','hanyu','andygaga1','高兴913','comedian','michaelliuyang','494079223-323994','wshxf_ok','liboprince','maruimcr','simazixuan','bulanjing','qjmsummer','lively98 ','Zhangllzhangll','Pzhpigs','小凯伊妈咪','C21jufe','胡婧博19860404','长翅膀的鲤鱼','christina_li')

tb = na.omit(tb)
#  baby_sku.txt
tb=fread('/rstudio/zhurui/1.txt',sep='\t',nrow=10000,verbose = T,header=T,na.strings=c("NA","NULL"))

col = sapply(tb, class)
tb=fread('/rstudio/zhurui/1.txt',sep='\t',verbose = T,colClasses=col,header=T,na.strings=c("NA","NULL"))
colnames(tb) = gsub('leijixiaofei.','',colnames(tb),fixed=T)

tb=read.csv("/rstudio/zhurui/baby_sku.txt",sep='\t',header=T)

require(data.table)
require(RJDBC)
require(rJava)
library(RHive)

f13 = list.files("/usr/lib/hive/lib",pattern="*jar$", full.names=T)
f14 = list.files("/usr/lib/hadoop/lib", pattern="*jar$",full.names=T)
f15 = list.files("/usr/lib/hadoop", pattern="*jar$",full.names=T)
f16 = union(f13,f14)
f17 = union(f15,f16)
drv = JDBC('org.apache.hive.jdbc.HiveDriver',f17)
conn1 = dbConnect(drv,sprintf('jdbc:hive2://172.25.24.2:10001/base_ads;'))
tb=system.time(dbGetQuery(conn1,"select cert_no,age,sex,phone_city,user_level,buy_m,yr_order_cnt,yr_order_amt,yr_max_amt,yr_order_amt_rank,yr_avgmonth_cnt,mth_order_cnt6,mth_order_amt6,mth_order_cnt3,mth_order_amt3,yr_maxmonth_amt,city_ofen,bankcard_cnt,cdt_card_cnt,onli_pay_cnt,onli_pay_amt,offli_pay_cnt,offli_pay_amt from base_ads.cert_acct_sum "))

rhive.init(hiveHome = '/usr/lib/hive',hiveLib = '/usr/lib/hive/lib',
           hadoopHome = '/usr/lib/hadoop' ,hadoopConf = '/usr/lib/hadoop/etc/hadoop',
           hadoopLib = '/usr/lib/hadoop/lib',verbose = F)
rhive.connect(host = "172.25.24.11",port=10001,hiveServer2 = TRUE,defaultFS = "hdfs://wangyin-hc-1/",updateJar = FALSE,user = 'fxff' ,password= NULL) 
rhive.list.databases()
rhive.list.tables()
tb = rhive.big.query('select cert_no,age,sex,phone_city,user_level,buy_m,yr_order_cnt,yr_order_amt,yr_max_amt,yr_order_amt_rank,yr_avgmonth_cnt,mth_order_cnt6,mth_order_amt6,mth_order_cnt3,mth_order_amt3,yr_maxmonth_amt,city_ofen,bankcard_cnt,cdt_card_cnt,onli_pay_cnt,onli_pay_amt,offli_pay_cnt,offli_pay_amt from base_ads.cert_acct_sum ',memLimit=1024*1024*1024*100)
tb = rhive.query('select * from tmp.baby_sku ')
rhive.close()

now()
wd = getwd()
setwd('/rstudio')
tb=rhive.big.query('select cert_no,age,sex,phone_city,user_level,buy_m,yr_order_cnt,yr_order_amt,yr_max_amt,yr_order_amt_rank,yr_avgmonth_cnt,mth_order_cnt6,mth_order_amt6,mth_order_cnt3,mth_order_amt3,yr_maxmonth_amt,city_ofen,bankcard_cnt,cdt_card_cnt,onli_pay_cnt,onli_pay_amt,offli_pay_cnt,offli_pay_amt from base_ads.cert_acct_sum ',memLimit=1024*1024*1024*100)
