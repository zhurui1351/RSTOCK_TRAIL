rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')

dou1 = read_dou1_d_wind()
dou1$votile = dou1$Close - dou1$Open

shindex = read_sh000001()
shindex$votitleratio = (shindex$Close - shindex$Open) / shindex$Open
dates = index(shindex)[which(shindex$votitleratio < -0.055)]

id = c()
for(d in dates)
{
  i = which(index(dou1) == d)
  if(length(i) == 0) next
  i = i + 2
  id = c(id,i)
}

subdou1 = dou1[id,]
length(dou1$votile[dou1$votile > 0]) / length(dou1$votile)

length(subdou1$votile[subdou1$votile > 0]) / length(subdou1$votile)

#汇市冲击
usdcny_d = readusdcny_d()
usdcny_d_change = diff(usdcny_d) * 10000

dates = usdcny_d_change[usdcny_d_change > 100]
