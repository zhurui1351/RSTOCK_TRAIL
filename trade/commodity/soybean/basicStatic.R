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
testMonthPeriod(dou1,detail = T)
testMonthPeriod(dou_us)

tradeRule = backTestBySnRule(rep(0,12),dou1,buyday=8,sellday=9,short = T,verbose = F,tradeDays = 12,type='bm',computePosition=volatilityPositionForTotalEq(dou1day,n=3,ratio=0.01))
reportpath = "D:/myreport_m"
f = 'dou1day'
reports = SnReport(dou1day,confProp=0.7,profitratio = 0,prune=3,initEq = 10000,drawdownratio = -5,path=paste(reportpath,'/',f,sep=""),type='bm',tradeDays = 12,computePosition=fixPosition(size=1))

