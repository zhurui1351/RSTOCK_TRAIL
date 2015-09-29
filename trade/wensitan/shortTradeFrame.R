rm(list=ls(all=T))
require(quantmod)
require(TTR)
require('dygraphs')
require('lubridate')
require('dplyr')

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/help')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/trade')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/analysis')

shindex = readSHindex()
allcodes = readallstockforday()
mg = mget(allcodes)
print(now())

xxs = shindex['201101']
end = index(xxs)
ld = lapply(end,function(x){
  print(x)
  l = bounceAfterFall(as.character(x),mg,ratio = -0.1,shindex=shindex)#growRatioGreaterThanDegree(as.character(x),mg,ratio=0.07)#filterDaoChuizi(as.character(x),mg)#
  l=Filter(function(x){!is.null(x)},l)
  if(length(l) > 0)
  {
    l = list(l)
    names(l) = as.character(x)
    return(l)
  }
  return(NULL)
})
print(now())

l = Filter(function(x){ ll = x[[1]]
                        length(ll)!=0},ld)
names(l)=sapply(l,function(x){return(names(x))})
names(l) = strftime(names(l),"%Y-%m-%d")
save(l,file='short.Rdata')

#统计list里面的每个选项的一些分布特征
sepdays = c()

for(i in 1:length(l))
{
  p = l[[i]]
  pdate = names(p) 
  print(pdate)
  
  p = p[[1]]
  sdays =  sapply(p,function(x,pdate){
    pname = x[[1]]
    print(pname)
    sep =afterNdaysProfit(pname,pdate,1)# findtimeGapWhengrowToSomeDegree(pname,pdate,0.02)# 
    return(sep)
  },pdate)
  print(sdays)
  sepdays = c(sdays,sepdays)
  
}
sepdays =  unlist(sepdays)
sepdays = sepdays[!is.na(sepdays)]
length(sepdays[sepdays<5 & sepdays>0]) / length(sepdays)

#产生回测记录

records = list()
print(now())
for(i in 1:length(l))
{
  p = l[[i]]
  pdate = names(p) 
  print(pdate)
  
  p = p[[1]]
  trades =  lapply(p,function(x,pdate){
    pname = x[[1]]
  #  print(pname)
    record =afterNPeriod(pname,pdate,n=2)#afterNatrExit(pname,pdate,0.5,0.5,0.5)#
    return(record)
  },pdate)
  records = append(records,trades)
 # print(trades)
}

records = as.data.frame(do.call('rbind',records))
records = subset(records,!is.na(records[,'code']) & Open>0 & Close>0)
#records = subset(records, Open>10 & Open < 20)

print(now())

profit = as.numeric(records[,'Close']) - as.numeric(records[,'Open'])
sum(profit)
max(profit)
min(profit)
length(profit[profit>0]) / length(profit)

