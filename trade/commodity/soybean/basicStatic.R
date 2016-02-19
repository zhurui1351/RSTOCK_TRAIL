rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/globaltool/data_handle/commodity/soybean')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/SNPACKAGE/R')

dou1 = read_dou1_ch()
dou2 = read_dou2_ch()

dou_us = read_s_f_us()

oldpars = par(mfrow=c(3, 1))

plot(Cl(dou1))
plot(Cl(dou2))
plot(Cl(dou_us['1990/']))

par(oldpars)

#sn 分析
testMonthPeriod(dou1,detail = T,from = '1990',to='2016')
testMonthPeriod(dou_us[,1:4],detail=T,from='1950')

SNRules(dou1,prune = 1,type='iw',tradeDays = 5,confProp=0.5)
SNRules(dou1,prune = 1,type='bm',tradeDays = 12)


#牛熊分析

#特殊事件分析

#特殊日历分析

#日内模型分析