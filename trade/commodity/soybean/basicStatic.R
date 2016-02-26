rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/globaltool/data_handle/commodity/soybean')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/SNPACKAGE/R')

dou1 = read_dou1_ch()
dou2 = read_dou2_ch()

dou_us = read_s_f_us()
dou_us_quandl = read_s1_quandl()
colnames(dou_us_quandl) = c("Open","High","Low","Close","Volume","preopeninterest")

oldpars = par(mfrow=c(3, 1))

plot(Cl(dou1))
plot(Cl(dou2))
plot(Cl(dou_us['1990/']))

par(oldpars)

#sn 分析
testMonthPeriod(dou1,detail = T,from = '1990',to='2016')
testMonthPeriod(dou_us[,1:4],detail=T,from='1950')
testMonthPeriod(dou_us_quandl[,1:4],detail=T,from='1950')

SNRules(dou1,prune = 1,type='iw',tradeDays = 5,confProp=0.5)
SNRules(dou1,prune = 1,type='bm',tradeDays = 12)

rule = rep(0,12)
rule[5] = 1
rules = list(rule)
stockdata = dou1
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

#特殊事件分析

#特殊日历分析

#日内模型分析