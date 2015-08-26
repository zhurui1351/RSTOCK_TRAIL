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
mg = mget(lookups)

xxs = shindex['2015']
end = index(xxs)
ld = lapply(end,function(x){
  print(x)
  l = filterDaoChuizi(as.character(x),mg)
  l = list(l)
  names(l) = as.character(x)
  return(l)
})
l = Filter(function(x){ ll = x[[1]]
                        length(ll)!=0},ld)
names(l)=sapply(l,function(x){return(names(x))})


