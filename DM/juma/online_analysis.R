path = 'D:/jumpdata/user_analysis.csv'
weixindt = read.csv(path,head=T,stringsAsFactors = F,na.strings = c("NA",' ','\\',''),
         sep=',',strip.white=T,blank.lines.skip=T)
weixindt$时间 = as.Date(weixindt$时间)
day_stat = basic_user_stat(cusdt,orderdt)
day_stat$死亡用户数 =c(0,diff(day_stat$死亡用户数)) 
day_stat = subset(day_stat,日期 %in% weixindt$时间)
weixindt = subset(weixindt,时间 %in% day_stat$日期)

cor(day_stat$新增客户,weixindt$新关注人数)
cor(day_stat$老客户,weixindt$新关注人数)
cor(day_stat$订单量,weixindt$新关注人数)
cor(day_stat$订单量,weixindt$净增关注人数)
cor(day_stat$死亡用户数,weixindt$净增关注人数)
cor(day_stat$死亡用户数,weixindt$取消关注人数)
