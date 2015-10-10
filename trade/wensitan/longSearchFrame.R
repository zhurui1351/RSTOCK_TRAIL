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
mg = mget(allcodes)

years = as.character(1990:2015) 
ld = lapply(years,function(x){
  print(x)
  l = followAfterUp(as.character(x),mg,ratio = 0.12,everdayratio = 0.05,upratio = 0.08,shindex=shindex,shfallratio=-0.10)
  l=Filter(function(x){!is.null(x)},l)
  if(length(l) > 0)
  {
    l = list(l)
    names(l) = as.character(x)
    return(l)
  }
  return(NULL)
})