require(quantmod)
require(TTR)
library(blotter)
require(lubridate)
require(dygraphs)

readallDayData()

Close_dou1 = Cl(to.weekly(dou1day))
Close_dou2 = Cl(dou2day)


dygraph(Close_dou1)
tradeRule = backTestBySnRule(rep(0,12),dou1day,buyday=8,sellday=9,short = T,verbose = F,tradeDays = 12,type='bm',computePosition=volatilityPositionForTotalEq(dou1day,n=3,ratio=0.01))
reportpath = "D:/myreport_m"
f = 'dou1day'
reports = SnReport(dou1day,confProp=0.7,profitratio = 0,prune=3,initEq = 10000,drawdownratio = -5,path=paste(reportpath,'/',f,sep=""),type='bm',tradeDays = 12,computePosition=fixPosition(size=1))

