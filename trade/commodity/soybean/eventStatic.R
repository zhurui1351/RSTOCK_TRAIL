rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')

dou1 = read_dou1_d_wind()
dou1$votile = dou1$Close - dou1$Open
dou1$maxvotile = dou1$High - dou1$Low

shindex = read_sh000001()
shindex$votitleratio = (shindex$Close - shindex$Open) / shindex$Open
dates = index(shindex)[which(shindex$votitleratio > 0.04)]

id = c()
for(d in dates)
{
  i = findInterval(d,index(dou1))
  #i = which(index(dou1) == d)
  if( i == 0) next
  id = c(id,i)
}

result = data.frame()
for(i in id)
{
  sep  = 0
  if((i+sep) > length(index(dou1)))
  {
    sep = length(index(dou1)) - i
  }
  if(sep == 0)
  {
    pre = (i-1)
    after = (i+1)
  }
  else
  {
    pre = (i-sep):(i-1)
    after = (i+1) : (i+sep)
  }
  
  prevotile = dou1[pre,]$votile
  aftervotile = dou1[after,]$votile
  
  premean = mean(prevotile)
  aftermean = mean(aftervotile)
  preratio = length(aftervotile[prevotile>0])/length(prevotile)
  afterratio = length(aftervotile[aftervotile>0])/length(aftervotile)
  result = rbind(result,data.frame(meanchg = aftermean - premean, upratiochg = afterratio-preratio))
}


subdou1 = dou1[id+1,]
nrow(subdou1)
length(dou1$votile[dou1$votile > 0]) / length(dou1$votile)

length(subdou1$votile[subdou1$votile > 0]) / length(subdou1$votile)

#汇市冲击
usdcny_d = readusdcny_d()
usdcny_d_change = diff(usdcny_d) * 10000

dates = usdcny_d_change[usdcny_d_change > 100]
