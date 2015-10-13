#http://stockcharts.com/school/doku.php?id=chart_school:trading_strategies:sector_rotation_roc
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
shindex_year = to.yearly(shindex)
allcodes = readallpuredata(period='years')

omit=lapply(allcodes,function(x){
  e = parent.env(environment())
  price = get(x)
  price$volatile = (Cl(price) - Op(price) ) / Op(price)
  assign(x,price,envir=e)
})

mg = mget(allcodes)


years = as.character(1995:2015) 
ld = lapply(years,function(x){
  print(x)
  l = sortBeststocks(as.character(x),mg)
  return(l)
})
names(ld) = years

#quarters = as.character(seq(as.POSIXct('1995-01-01'),as.POSIXct('2014-12-31'),by='quarters'))
#quarters = substr(quarters,1,7)
shindex_quarter = to.period(shindex,period='quarters')
quarters = as.character(index(shindex_quarter['1995/']))
quarters = substr(quarters,1,7)
allcodes = readallpuredata(period='quarters')
omit=lapply(allcodes,function(x){
  e = parent.env(environment())
  price = get(x)
  price$volatile = (Cl(price) - Op(price) ) / Op(price)
  assign(x,price,envir=e)
})

mg = mget(allcodes)
ld_q = lapply(quarters,function(x){
  print(x)
  l = sortBeststocks(as.character(x),mg)
  return(l)
})
names(ld_q) = quarters