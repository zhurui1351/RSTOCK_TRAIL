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
shindex_m = to.monthly(shindex)

shindex$sma = SMA(Cl(shindex),150)


special = list(c(from = '1995-05-18',to='1995-05-22'))

sapply(bear, function(x){as.numeric(as.Date(x['to']) - as.Date(x['from'])) })
sapply(bull, function(x){as.numeric(as.Date(x['to']) - as.Date(x['from'])) })

bulllist  = find_bull(upratio = 1,downratio = -0.3,shindex = shindex)

bearlist = find_bear(upratio = 0.2,downratio = -0.4,shindex = shindex)

#看图分析
for(i in 1:length(bulllist))
{
  bullfrom = bulllist[[i]]['from']
  bullto = bulllist[[i]]['to']
  bullend = bulllist[[i]]['end']
  statdate = paste(bullfrom,bullto,sep='/')
  print(statdate)
  print(sd(Delt(Cl(shindex[statdate])),na.rm=T))
  print(mean(Delt(Cl(shindex[statdate])),na.rm=T))
  plotdata = cbind(Cl(shindex[statdate]),shindex[paste(from,end,sep='/')]$sma)
  plot(plotdata)
  tmp = scan()
}

for(i in 1:length(bearlist))
{
  bearfrom = bearlist[[i]]['from']
  bearto = bearlist[[i]]['to']
  bearend = bearlist[[i]]['end']
  statdate = paste(bearfrom,bearto,sep='/')
  print(statdate)
  print(sd(Delt(Cl(shindex[statdate])),na.rm=T))
  print(mean(Delt(Cl(shindex[statdate])),na.rm=T))
  plotdata = cbind(Cl(shindex[statdate]),shindex[paste(from,end,sep='/')]$sma)
  plot(plotdata)
  tmp = scan()
}



shindex$preclose = lag(shindex$Close,1)
shindex$presma = lag(shindex$sma,1)
cnt = ifelse(shindex$preclose < shindex$presma & shindex$Close >shindex$sma,1,0 )
sum(as.numeric(cnt),na.rm = T)
breakout_sma = cnt[which(cnt == 1),]

cnt = ifelse(shindex$Close > shindex$sma,1,0 )
sum(as.numeric(cnt),na.rm = T)
status_sma = cnt[which(cnt == 1),]
