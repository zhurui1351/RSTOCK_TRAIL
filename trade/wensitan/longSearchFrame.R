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
  price$volatile = (Cl(price) - Op(price) ) / Cl(price)
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
