rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/SNPACKAGE/R',encoding='utf8')

dou1 = read_dou1_d_wind()

dou_us = read_s_f_us()
dou_us_quandl = read_s1_quandl()
colnames(dou_us_quandl) = c("Open","High","Low","Close","Volume","preopeninterest")

doubo_m = read_m_1m_taobao()
douyou_m = read_y_1m_taobao()
corp_m = read_c_1m_taobao()
dou1_m = read_s1_1m_taobao()

dou1_day = to_day(dou1_m)
doubo_day = to_day(doubo_m)
douyou_day = to_day(douyou_m)
corp_day = to_day(corp_m)

shindex = read_sh000001()

oldpars = par(mfrow=c(3, 1))
plot(Cl(dou1))
plot(Cl(dou_us['1990/']))

par(oldpars)

#sn 分析
dou1 = dou1_day
testMonthPeriod(dou1[,1:4],detail = T,from = '1990',to='2016',strict=F)
testMonthPeriod(dou_us[,1:4],detail=T,from='1950')
testMonthPeriod(dou_us_quandl[,1:4],detail=T,from='1950')

SNRules(dou1,prune = 1,type='iw',tradeDays = 5,confProp=0.5)
SNRules(dou1,prune = 1,type='bm',tradeDays = 12)

rule = rep(0,12)
rule[5] = 1
rules = list(rule)
stockdata = dou1[,1:4]
stockdata = initialData(stockdata,type='bm')
getBasePropByRules(rules,stockdata,tradeDays)
subset(stockdata,sec == 10 )

stockdata = dou1
stockdata$presma5 = lag(SMA(stockdata$Close,5),1)
stockdata$preclose = lag(stockdata$Close,1)

stockdata$presma30 = lag(SMA(stockdata$Close,10),1)

months = as.numeric(strftime(index(stockdata),format='%m'))

testdata = as.data.frame(stockdata)
testdata$date = substr(as.character(index(stockdata)),1,7)

testdata = ddply(testdata,"date",transform,num=1:length(date))
testdata$profit = testdata$Close - testdata$Open
testdata$flag = ifelse(testdata$presma5 > testdata$presma30,1,-1)
xx = subset(testdata,substr(date,6,7) == '10' & num == 1)
nrow(subset(xx,sign(profit) == sign(flag)))

#牛熊分析
find_bull(upratio = 0.2,downratio = -0.1,shindex = na.omit(dou1[,1:4]))

#特殊事件分析 usda调高全球产量



#特殊日历分析

#日内模型分析

#基本统计
pricedata = dou1_m
days = as.character(unique(as.Date(index(pricedata))))
vs = c()
for(day in days)
{
  print(day)
  p = pricedata[day]
  if(nrow(p) ==0) next
  perdstart = paste(day,'09:00:00')
  perdend = paste(day,'15:00:00')
  perd = paste(perdstart,perdend,sep='/')
  nperd = nrow(p[perd])
  if(nperd == 0) next
  p = p[perd]
  
  dayvotile = max(p$High) - min(p$Low)
  time1 = paste(day,'09:01:00')
  time2 = paste(day,'10:00:00')
  tp = paste(time1,time2,sep='/')
  ptp = p[tp]
  votile = max(ptp$High) - min(ptp$Low)
  vs = c(vs,votile/dayvotile)
  
}

vdays = c()
for(day in days)
{
  print(day)
  p = pricedata[day]
  if(nrow(p) ==0) next
  perdstart = paste(day,'09:00:00')
  perdend = paste(day,'15:00:00')
  perd = paste(perdstart,perdend,sep='/')
  nperd = nrow(p[perd])
  if(nperd == 0) next
  p = p[perd]
  pi = p[1,]
  if((pi$High - pi$Open) > 30 || (pi$Open - pi$Low) < -30)
  {
    vdays = c(vdays,day)
  }
  
}

# 回归
cl_dou1 = dou1_day$Close
cl_douyou = douyou_day$Close
cl_doubo = doubo_day$Close
cl_corp = corp_day$Close

cl = merge(cl_dou1,cl_douyou,cl_doubo,cl_corp)
cl = na.omit(cl)
names(cl)= c('dou','douyou','doubo','corp')
cl = as.data.frame(cl)

f = lm(dou~.,data = cl[1:300,])

idx = data.frame()
for(i in 301:nrow(cl))
{
  f = lm(dou~.,data = cl[(i-300):(i-1),])
  se = sd(f$residuals)
  pv = predict(f,cl[i,c(2,3,4)])
  tv = cl[i,1]
  if(abs(tv - pv) > 2 * se)
  {
    r = data.frame(i=i,sp = pv - tv)
    idx = rbind(idx,r)
  }
}

#回测
ratio = 2
records = data.frame()
first = F
ishold = F
long = F
short = F
type = ''
x = 0
num = 200
for(i in (num+1):nrow(cl))
{
  f = lm(dou~.,data = cl[(i - num):(i-1),])
  se = sd(f$residuals)
  pv = predict(f,cl[i,c(2,3,4)])
  tv = cl[i,1]
  it = rownames(cl[i,])
  price = dou1_day[it]
  gap = pv - tv
  
  if(short || long)
  {
     open = as.numeric(price$Open)
     type = ifelse(short==T,'short','long')
     short = F
     long = F
     ishold = T
     entertime = it
     x = i
  }
 
  if(abs(gap) > ratio * se && ishold == F)
  {
    if(gap > 0)
    {
      long = T
    }
    else
    {
      short = T
    }
  }
 
  if(ishold == T)
  {
    if(abs(gap) > ratio * se)
    {
      next
    }
    outit = rownames(cl[i+1,])
    outprice = dou1_day[outit]   
    out = as.numeric(outprice$Open)
    outtime = outit
    r = data.frame(entertime = entertime,open=open,out=out,outtime=outtime,type = type,profit = ifelse(type=='long',out-open,open-out),i=x)
    records = rbind(records,r)
    ishold = F
  }
}

# 5分钟数据
cl_dou1_m = Cl(to.minutes15(dou1_m))
cl_douyou_m = Cl(to.minutes15(douyou_m))
cl_doubo_m = Cl(to.minutes15(doubo_m))
cl_corp_m = Cl(to.minutes15(corp_m))

cl_m = merge(cl_dou1_m,cl_douyou_m,cl_doubo_m)
cl_m = na.omit(cl_m)
names(cl_m)= c('dou','douyou','doubo')
cl_m = as.data.frame(cl_m)

f = lm(dou~.,data = cl_m[1:300,])

dou1 = to.minutes15(dou1_m)


ratio = 2
records = data.frame()
first = F
ishold = F
long = F
short = F
type = ''
x = 0
num = 300
for(i in (num+1):nrow(cl_m))
{
  f = lm(dou~.,data = cl_m[(i - num):(i-1),])
  se = sd(f$residuals)
  pv = predict(f,cl_m[i,c(2,3)])
  tv = cl_m[i,1]
  it = rownames(cl_m[i,])
  price = dou1[it]
  gap = pv - tv
  
  if(short || long)
  {
    open = as.numeric(Op(price))
    type = ifelse(short==T,'short','long')
    short = F
    long = F
    ishold = T
    entertime = it
    x = i
  }
  
  if(abs(gap) > ratio * se && ishold == F)
  {
    if(gap > 0)
    {
      long = T
    }
    else
    {
      short = T
    }
  }
  
  if(ishold == T)
  {
    if(abs(gap) > ratio * se)
    {
      next
    }
    outit = rownames(cl_m[i+1,])
    outprice = dou1[outit]   
    out = as.numeric(Op(outprice))
    outtime = outit
    r = data.frame(entertime = entertime,open=open,out=out,outtime=outtime,type = type,profit = ifelse(type=='long',out-open,open-out),i=x)
    records = rbind(records,r)
    ishold = F
  }
}