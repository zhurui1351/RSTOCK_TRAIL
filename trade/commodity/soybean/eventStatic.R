rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/eventAnalysis')

dou1 = read_dou1_d_wind()
dou1$votile = dou1$Close - dou1$Open
dou1$maxvotile = dou1$High - dou1$Low

#A股冲击
shindex = read_sh000001()
shindex$votitleratio = Delt(shindex$Close)#(shindex$Close - shindex$Open) / shindex$Open

dates = index(shindex)[which(shindex$votitleratio  <  -0.05)]
basicEventStatic(dou1[,1:4],dates,30,1:30)

dates = index(shindex)[which(shindex$votitleratio  >  0.05)]
basicEventStatic(dou1[,1:4],dates,30,3)

#汇市冲击
usdcny_d = readusdcny_d()
usdcny_d_change = diff(usdcny_d) * 10000

#贬值
dates =index(usdcny_d_change[usdcny_d_change > 100]) 
basicEventStatic(dou1[,1:4],dates,30,1:10)
#升值
dates =index(usdcny_d_change[usdcny_d_change < -100]) 
basicEventStatic(dou1[,1:4],dates,30,2)

#价格异常波动
dou1_p = na.omit(dou1[,c('Open','High','Low','Close','votile')])
dou1_p$abssmav = lag(SMA(abs(dou1_p$votile),30,na.rm=T))
votile_change = dou1_p$votile/dou1_p$abssmav
dates = index(votile_change[votile_change < -2]) 
basicEventStatic(dou1[,1:4],dates,30,2)

dates = index(votile_change[votile_change > 2]) 
basicEventStatic(dou1[,1:4],dates,30,2)

#隔夜美豆异常波动
s1_d = read_s1_d_wind()
s1_d$votile =  s1_d$Close - s1_d$Open
s1_p = na.omit(s1_d[,c('Open','High','Low','Close','votile')])
s1_p$abssmav = lag(SMA(abs(s1_p$votile),30,na.rm=T))
votile_change = s1_p$votile/s1_p$abssmav
dates = index(votile_change[votile_change < -1.5])

basicEventStatic(dou1[,1:4],dates,30,1)

dates = index(votile_change[votile_change > 1.5])
basicEventStatic(dou1[,1:4],dates,30,2)


#经济事件 社会政治事件 自然事件 疫情 新奥尔良飓风灾难 hurricanes 季节性 气候