rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/SNPACKAGE/R',encoding='utf8')

dou1 = read_dou1_d_wind()

dou_us = read_s_f_us()
dou_us_quandl = read_s1_quandl()
colnames(dou_us_quandl) = c("Open","High","Low","Close","Volume","preopeninterest")

shindex = read_sh000001()

oldpars = par(mfrow=c(3, 1))
plot(Cl(dou1))
plot(Cl(dou_us['1990/']))

par(oldpars)

#sn 分析
testMonthPeriod(dou1[,1:4],detail = T,from = '1990',to='2016',strict=F)
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
find_bull(upratio = 0.2,downratio = -0.1,shindex = na.omit(dou1[,1:4]))

#特殊事件分析 usda调高全球产量



#特殊日历分析

#日内模型分析