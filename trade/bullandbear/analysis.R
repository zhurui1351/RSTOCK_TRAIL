rm(list=ls(all=T))
require(quantmod)
require(TTR)
require('dygraphs')
require('lubridate')
require('dplyr')
require('dygraphs')
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/help')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/bullandbear/help')

shindex = readSHindex()
shindex_w = to.weekly(shindex)
colnames(shindex_w) = gsub('shindex.','',colnames(shindex_w),fixed=T)
shindex_m = to.monthly(shindex)
colnames(shindex_m) = gsub('shindex.','',colnames(shindex_m),fixed=T)


#special = list(c(from = '1995-05-18',to='1995-05-22'))


myindex = shindex_w

bulllist  = find_bull(upratio = 1,downratio = -0.3,shindex = myindex)

bearlist = find_bear(upratio = 0.2,downratio = -0.4,shindex = myindex)


#看图分析
for(i in 1:length(bulllist))
{
  bullfrom = bulllist[[i]]['from']
  bullto = bulllist[[i]]['to']
  bullend = bulllist[[i]]['end']
  statdate = paste(bullfrom,bullto,sep='/')
  print(statdate)
  print(sd(Delt(myindex[statdate]$Close),na.rm=T))
  print(mean(Delt(myindex[statdate]$Close),na.rm=T))
  plotdata = cbind(Cl(myindex[statdate]),myindex[statdate]$sma)
  chartSeries(OHLC(myindex),TA=c(addSMA(n=12,col = "green"),addSMA(n=4,col = "red")),subset=paste(bullfrom,bullend,sep='::'))

  addTA(Cl(myindex[statdate]), on=1, col='yellow')
  
  tmp = scan()
}

for(i in 1:length(bearlist))
{
  bearfrom = bearlist[[i]]['from']
  bearto = bearlist[[i]]['to']
  bearend = bearlist[[i]]['end']
  statdate = paste(bearfrom,bearto,sep='/')
  print(statdate)
  print(sd(Delt(myindex[statdate]$Close),na.rm=T))
  print(mean(Delt(myindex[statdate]$Close),na.rm=T))
  plotdata = cbind(Cl(myindex[statdate]),myindex[statdate]$sma)
  chartSeries(myindex,TA=c(addSMA(n=100,col = "green"),addSMA(n=30,col = "red")),subset=paste(bearfrom,bearend,sep='::'))
  tmp = scan()
}


####basic stat
myindex = shindex_w
myindex$sma = SMA(myindex$Close,n = 12)
myindex$preclose = lag(myindex$Close,1)
myindex$presma = lag(myindex$sma,1)
cnt = ifelse(myindex$preclose < myindex$presma & myindex$Close >myindex$sma,1,0 )
sum(as.numeric(cnt),na.rm = T)
breakout_sma = cnt[which(cnt == 1),]

bulist = bulllist[1:(length(bulllist)-1)]
from_bull = sapply(bulist, function(x){return(as.character(x['from']))})

sapply(bulist, function(x){as.numeric(as.Date(x['to']) - as.Date(x['from'])) })

#标注关心的状态转移的阶段
for(i in 1: length(bulist))
{
  i = i + 1
  l = bulist[[i]]
  tmpdate = paste(l['from'],l['to'],sep='/')
  tmpdate1 = paste(l['from'],l['to'],sep='::')
  
  i1 = which(index(myindex) == l['from'])
  i2 = which(index(myindex) == l['to'])
  
  revindex = as.data.frame(myindex)[i2:i1,]
  
  print(find_bull(upratio = 0.3,downratio = -0.1,myindex[tmpdate])) 
  dygraph(myindex[tmpdate]$Close)
  tmp = scan()
}

from_bull = c('1992-12-11','1994-07-29','1996-01-19','"1999-12-30','2007-06-08','2008-10-31','2014-07-18')


for(i in 1 : breakout_sma)
{
   brktime = index(breakout_sma[i])
   for(j in 1 : length(bulist))
   {
     
   }
}
