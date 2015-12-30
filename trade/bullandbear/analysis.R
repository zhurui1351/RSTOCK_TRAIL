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
#上涨100%，跌幅 30%
bear = list(c(from = '1991-01-02',to='1992-05-26'),c(from = '1992-11-17',to='1993-02-16'),c(from = '1994-07-29',to='1994-09-13')
           ,c(from = '1996-01-19',to='1997-05-12'),c(from = '1999-05-19',to='2001-06-14')
            ,c(from = '2005-06-06',to='2007-10-16'),c(from = '2008-10-28',to='2009-08-04'),c(from = '2014-07-22',to='2015-06-12'))

bull = list(c(from = '1992-05-26',to='1992-11-17'),c(from = '1993-02-16',to='1994-07-29'),c(from = '1994-09-13',to='1996-01-19')
            ,c(from = '1997-05-12',to='1999-05-18'),c(from = '2001-06-14',to='2005-06-06'),c(from = '2007-10-16',to='2008-10-28')
            ,c(from = '2009-08-04',to='2010-07-02'))

sapply(bear, function(x){as.numeric(as.Date(x['to']) - as.Date(x['from'])) })
sapply(bull, function(x){as.numeric(as.Date(x['to']) - as.Date(x['from'])) })



bearlist  = find_bull(upratio = 1,downratio = -0.3,shindex = shindex)
bearlist = bearlist[1:(length(bearlist)-1)]
from = bearlist[[3]]['from']
to = bearlist[[3]]['to']
end = bearlist[[3]]['end']

beardata = cbind(Cl(shindex[paste(from,end,sep='/')]),shindex[paste(from,end,sep='/')]$sma100)

dygraph(beardata)

shindex$preclose = lag(shindex$Close,1)
shindex$presma = lag(shindex$sma,1)
cnt = ifelse(shindex$preclose < shindex$presma & shindex$Close >shindex$sma,1,0 )
sum(as.numeric(cnt),na.rm = T)
breakout_sma = cnt[which(cnt == 1),]

cnt = ifelse(shindex$Close > shindex$sma,1,0 )
sum(as.numeric(cnt),na.rm = T)
status_sma = cnt[which(cnt == 1),]