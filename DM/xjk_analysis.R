require('data.table')
require('rpart')
library('arules') 
require('rpart.plot')
path = '/rstudio/wangmingxin/kyc_xjk2.txt'
#tb=fread(path,sep='\t',nrow=1000,verbose = T,na.strings=c("NA","NULL",'NONE'),header = T)
#col = sapply(tb, class)
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE','unknown'),header = T,colClasses = rep('character',88))
colnames(tb) = gsub('kyc_xjk2.','',colnames(tb),fixed = T)
colname = colnames(tb)
f = as.formula('xjk_flag ~   gender + age ')

tbframe = as.data.frame(tb)
tbtest = as.data.frame(tbframe[1:50,c('xjk_flag','user_lv_cd','gender','age')],stringsAsFactors=T)


model.fit= rpart(f,tbtest, method="class")



path = '/rstudio/wangmingxin/kyc_xjk4.txt'
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE'),header = T,colClasses = rep('character',88))
colnames(tb) = gsub('kyc_xjk4.','',colnames(tb),fixed = T)
colname = colnames(tb)
tb = tb[,2:length(colname),with=F]

fit<- rpart (xjk_flag ~ prefer_bank + prefer_payplat +  has_baby + worktime_cnt_level
             + resttime_cnt_level  +eday_cnt_level + publicday_cnt_level ,tb,method="class")


path = '/rstudio/zhurui/xjk.txt'
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE'),header = T,colClasses = rep('character',88),stringsAsFactors=F)
colnames(tb) = gsub('kyc_xjk4.','',colnames(tb),fixed = T)
colname = colnames(tb)
tb = tb[,3:length(colname),with=F]
varset = c('jijin_cnt3_level','age','curr_prvc','month3_cnt_level','industry','lastbuy_today_level','user_lv_cd')
varset=colname[3:length(colname)]
tbda = as.data.frame(tb,stringsAsFactors=T)
tbtest = as.data.frame(tbda[,varset],stringsAsFactors=T)
tbtest[is.na(tbtest)] = 0
for(i in 1:ncol(tbtest))
{
  tbtest[,i] = as.factor(tbtest[,i])
}
gc()
rules <- apriori(tbtest, 
                 parameter = list(supp = 0.1, conf = 0.5, target = "rules"))

k = kmeans(tbtest,3)

path = 'xjk_cluster.txt'
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE'))#,colClasses = rep('character',95)
colnames(tb) = gsub('xjk_cluster_all.','',colnames(tb),fixed = T)

tt = tb[!is.na(jd_pin),]

tbda = as.data.frame(tt,stringsAsFactors=T)
#tbda = as.data.frame(tt[type != 'zombie_cus' & type!='sleep_cus',],stringsAsFactors=T)
varset = c('type','age','consume_level','his_addr_cnt_level')
tbtest = as.data.frame(tbda[,varset],stringsAsFactors=T)

for(i in 1:ncol(tbtest))
{
  tbtest[,i] = as.factor(tbtest[,i])
}
f = as.formula('type ~ . ')
fit<- rpart(f,data=tbtest,method="class",parms = list(split = "information"),control=rpart.control(minsplit=2, minbucket=10, cp=0.0001))
plot(fit)
text(fit)

rpart.plot(fit)

rpart.plot(fit, branch=1, branch.type=0, type=0,extra=0,
           box.col="green",
           border.col="blue", split.col="red")
fit1 = glm(f,tbtest,family="binomial")


type = tb[,type,by=type]
type = type[,1,with=F]

vars = c('consume_level','his_mobile_cnt_level')
for( i in 1:nrow(type))
{
  t = type[i,]
  print(t)
  tb1 = tb[type == t,]
  
  tb2_1 = table(tb1[,c(vars[1]),with=F])
  #print(tb2_1/sum(tb2_1))
  
  tb_2 = table(tb1[,c(vars[2]),with=F])
  print(tb_2/sum(tb_2))
  
  tb2_3 = table(tb1[,c(vars[1],vars[2]),with=F]) 
  #print(tb2_3 / sum(tb2_3))
}

#白条开通率
t1= tb[,length(bt_asset),by=type] 
t2 = tb[,length(bt_asset[!is.na(bt_asset)]),by=type] 
t = cbind(t1,t2)
t$v2 = t[,4,with=F] /  t[,2,with=F]

#平均年龄
tb[,mean(age,na.rm=T),by = type]

#独立分析
tb_chi = tbda
colname = colnames(tbda)
vars =  c('type','cust_acc_no','jd_pin','cert_no','user_log_acct','xjk_flag')
vars = setdiff(colname,vars)

for(v in vars)
{
  tbda[,v] = as.factor(tbda[,v])
  if(length(levels(tbda[,v])) < 2)
  {
    tbda[,v] = ifelse(is.na(tbda[,v]),-1,tbda[,v] )
  }
  p = chisq.test(tbda[,'type'],tbda[,v])
  if(p$p.value >= 0.01)
  {
    print(v)
  }
}


require('data.table')
require('rpart')
library('arules') 
require('rpart.plot')
path = '/rstudio/wangmingxin/kyc_xjk2.txt'
#tb=fread(path,sep='\t',nrow=1000,verbose = T,na.strings=c("NA","NULL",'NONE'),header = T)
#col = sapply(tb, class)
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE','unknown'),header = T,colClasses = rep('character',88))
colnames(tb) = gsub('kyc_xjk2.','',colnames(tb),fixed = T)
colname = colnames(tb)
f = as.formula('xjk_flag ~   gender + age ')

tbframe = as.data.frame(tb)
tbtest = as.data.frame(tbframe[1:50,c('xjk_flag','user_lv_cd','gender','age')],stringsAsFactors=T)


model.fit= rpart(f,tbtest, method="class")



path = '/rstudio/wangmingxin/kyc_xjk4.txt'
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE'),header = T,colClasses = rep('character',88))
colnames(tb) = gsub('kyc_xjk4.','',colnames(tb),fixed = T)
colname = colnames(tb)
tb = tb[,2:length(colname),with=F]

fit<- rpart (xjk_flag ~ prefer_bank + prefer_payplat +  has_baby + worktime_cnt_level
             + resttime_cnt_level  +eday_cnt_level + publicday_cnt_level ,tb,method="class")


path = '/rstudio/zhurui/xjk.txt'
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE'),header = T,colClasses = rep('character',88),stringsAsFactors=F)
colnames(tb) = gsub('kyc_xjk4.','',colnames(tb),fixed = T)
colname = colnames(tb)
tb = tb[,3:length(colname),with=F]
varset = c('jijin_cnt3_level','age','curr_prvc','month3_cnt_level','industry','lastbuy_today_level','user_lv_cd')
varset=colname[3:length(colname)]
tbda = as.data.frame(tb,stringsAsFactors=T)
tbtest = as.data.frame(tbda[,varset],stringsAsFactors=T)
tbtest[is.na(tbtest)] = 0
for(i in 1:ncol(tbtest))
{
  tbtest[,i] = as.factor(tbtest[,i])
}
gc()
rules <- apriori(tbtest, 
                 parameter = list(supp = 0.1, conf = 0.5, target = "rules"))

k = kmeans(tbtest,3)

path = 'xjk_cluster.txt'
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE'))#,colClasses = rep('character',95)
colnames(tb) = gsub('xjk_cluster_all.','',colnames(tb),fixed = T)

tt = tb[!is.na(jd_pin),]

tbda = as.data.frame(tt,stringsAsFactors=T)
#tbda = as.data.frame(tt[type != 'zombie_cus' & type!='sleep_cus',],stringsAsFactors=T)
varset = c('type','age','consume_level','his_addr_cnt_level')
tbtest = as.data.frame(tbda[,varset],stringsAsFactors=T)

for(i in 1:ncol(tbtest))
{
  tbtest[,i] = as.factor(tbtest[,i])
}
f = as.formula('type ~ . ')
fit<- rpart(f,data=tbtest,method="class",parms = list(split = "information"),control=rpart.control(minsplit=2, minbucket=10, cp=0.0001))
plot(fit)
text(fit)

rpart.plot(fit)

rpart.plot(fit, branch=1, branch.type=0, type=0,extra=0,
           box.col="green",
           border.col="blue", split.col="red")
fit1 = glm(f,tbtest,family="binomial")


type = tb[,type,by=type]
type = type[,1,with=F]

vars = c('consume_level','his_mobile_cnt_level')
for( i in 1:nrow(type))
{
  t = type[i,]
  print(t)
  tb1 = tb[type == t,]
  
  tb2_1 = table(tb1[,c(vars[1]),with=F])
  #print(tb2_1/sum(tb2_1))
  
  tb_2 = table(tb1[,c(vars[2]),with=F])
  print(tb_2/sum(tb_2))
  
  tb2_3 = table(tb1[,c(vars[1],vars[2]),with=F]) 
  #print(tb2_3 / sum(tb2_3))
}

#白条开通率
t1= tb[,length(bt_asset),by=type] 
t2 = tb[,length(bt_asset[!is.na(bt_asset)]),by=type] 
t = cbind(t1,t2)
t$v2 = t[,4,with=F] /  t[,2,with=F]

#平均年龄
tb[,mean(age,na.rm=T),by = type]

#独立分析
tb_chi = tbda
colname = colnames(tbda)
vars =  c('type','cust_acc_no','jd_pin','cert_no','user_log_acct','xjk_flag')
vars = setdiff(colname,vars)

for(v in vars)
{
  tbda[,v] = as.factor(tbda[,v])
  if(length(levels(tbda[,v])) < 2)
  {
    tbda[,v] = ifelse(is.na(tbda[,v]),-1,tbda[,v] )
  }
  p = chisq.test(tbda[,'type'],tbda[,v])
  if(p$p.value >= 0.01)
  {
    print(v)
  }
}


require('data.table')
path = '/rstudio/wangmingxin/xjk_fenlei1.txt'
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE'),header = T)
col = sapply(tb, class)
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL"),header = T,colClasses = col)
colnames(tb) = gsub('fenlei1.','',colnames(tb),fixed = T)
colname = colnames(tb)
colname = colname[2:length(colname)]
tb = tb[,colname,with=F]

tb_ti<-tb
k_ti=kmeans(tb_ti$total_income,2)
k_ti$size

require('data.table')
path = '/rstudio/wangmingxin/xjk_fenlei2.txt'
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL",'NONE'),header = T)
col = sapply(tb, class)
tb=fread(path,sep='\t',verbose = T,na.strings=c("NA","NULL"),header = T,colClasses = c('character',rep('numeric',3)))
colnames(tb) = gsub('fenlei2.','',colnames(tb),fixed = T)
colname = colnames(tb)
colname = colname[1:length(colname)]
tb = tb[,colname,with=F]

tb2<-tb
kt2<-kmeans(scale(tb2[,c(2,3,4),with=F]),2)

kt3<-kmeans(scale(tb2[,c(2,3,4),with=F]),3)
tb33<-data.table(tb2,cluster=kt3$cluster)
write.table(tb33,'/rstudio/wangmingxin/cluster.txt',quote=FALSE,sep=',',eol="\n",col.names=FALSE,fileEncoding = 'utf-8')

kt4<-kmeans(scale(tb2[,c(2,3,4),with=F]),4)
