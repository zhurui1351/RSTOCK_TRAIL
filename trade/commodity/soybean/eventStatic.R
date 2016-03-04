rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')

dou1 = read_dou1_d_wind()
dou1$votile = dou1$Close - dou1$Open
dou1$maxvotile = dou1$High - dou1$Low

shindex = read_sh000001()
shindex$votitleratio = Delt(shindex$Close)#(shindex$Close - shindex$Open) / shindex$Open
dates = index(shindex)[which(shindex$votitleratio  <  -0.055)]

id = c()
for(d in dates)
{
  i = findInterval(as.Date(d),index(dou1))
  #i = which(index(dou1) == d)
  if( i == 0) next
  id = c(id,i)
}

result = data.frame()
for(i in id)
{
  sep  = 10
  pre = (i - 10) : i
  if(i + 2 > length(index(dou1))) next
  after = i+2
  
  prevotile = dou1[pre,]$votile
  premean = mean(prevotile)
  
  
  preratio = length(prevotile[prevotile>0])/length(prevotile)

  aftervotile = dou1[after,]$votile
  
  
  result = rbind(result,data.frame(meanchg = premean, preratio = preratio , aftervotile = aftervotile ))
}


subdou1 = dou1[id+1,]
nrow(subdou1)
length(dou1$votile[dou1$votile > 0]) / length(dou1$votile)

length(subdou1$votile[subdou1$votile < 0]) / length(subdou1$votile)
prop.test(length(subdou1$votile[subdou1$votile < 0]),length(subdou1$votile))
#汇市冲击
usdcny_d = readusdcny_d()
usdcny_d_change = diff(usdcny_d) * 10000

dates =index(usdcny_d_change[usdcny_d_change > 100]) 
