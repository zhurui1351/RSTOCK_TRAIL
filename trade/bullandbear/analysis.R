#rm(list=ls(all=T))
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

shindex = readSHindex()
shindex_w = to.weekly(shindex)
shindex_m = to.monthly(shindex)

shindex$sma = SMA(Cl(shindex),150)


special = list(c(from = '1995-05-18',to='1995-05-22'))

sapply(bear, function(x){as.numeric(as.Date(x['to']) - as.Date(x['from'])) })
sapply(bull, function(x){as.numeric(as.Date(x['to']) - as.Date(x['from'])) })

bearlist  = find_bull(upratio = 1,downratio = -0.3,shindex = shindex)
bearlist = bearlist[1:(length(bearlist)-1)]
from = bearlist[[3]]['from']
to = bearlist[[3]]['to']
end = bearlist[[3]]['end']

beardata = cbind(Cl(shindex[paste(from,end,sep='/')]),shindex[paste(from,end,sep='/')]$sma)

dygraph(beardata)

shindex$preclose = lag(shindex$Close,1)
shindex$presma = lag(shindex$sma,1)
cnt = ifelse(shindex$preclose < shindex$presma & shindex$Close >shindex$sma,1,0 )
sum(as.numeric(cnt),na.rm = T)
breakout_sma = cnt[which(cnt == 1),]

cnt = ifelse(shindex$Close > shindex$sma,1,0 )
sum(as.numeric(cnt),na.rm = T)
status_sma = cnt[which(cnt == 1),]
