rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')

dou1 = read_dou1_d_wind()
dou1$votile = dou1$Close - dou1$Open
dou1$maxvotile = dou1$High - dou1$Low

#A股冲击
shindex = read_sh000001()
shindex$votitleratio = Delt(shindex$Close)#(shindex$Close - shindex$Open) / shindex$Open
dates = index(shindex)[which(shindex$votitleratio  <  -0.05)]
basicEventStatic(dou1[,1:4],dates,30,1)


#汇市冲击
usdcny_d = readusdcny_d()
usdcny_d_change = diff(usdcny_d) * 10000

dates =index(usdcny_d_change[usdcny_d_change > 100]) 
basicEventStatic(dou1[,1:4],dates,30,1:30)

#经济事件 社会政治事件 自然事件 疫情 新奥尔良飓风灾难 hurricanes 季节性 气候