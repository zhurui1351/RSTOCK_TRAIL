require(quantmod)
require(TTR)
library(blotter)
require(lubridate)
#for day data

readallDayData = function()
{
  path = "D:/data/commidity/soybean/d"
  files = dir(path)
  for(f in files)
  {
    
    fname = file.path(path,f)
    name = strsplit(f,'.',fixed=T)[[1]][1]
    print(name)
    priceData = read.zoo(fname,header=F, format = "%m/%d/%Y",sep=",",index.column=1)
    priceData = as.xts(priceData)
    colnames(priceData)<-c("Open","High","Low","Close","Volume","Hold",'Settlement')
    assign(name,priceData,pos=1)
  } 
}

#for minute data
path = "D:/data/commidity/soybean/m"
files = dir(path)
f = files[2]
fname = file.path(path,f)
priceData <- read.table(fname,sep=',',header=F,colClasses = rep(c( "character", "numeric"), c( 2, 5)))  
priceData$time = paste(priceData[,1],priceData[,2])
priceData$time = mdy_hm(priceData$time)
priceData = priceData[,3:ncol(priceData)]
priceData = xts(priceData[,1:7],priceData$time)
colnames(priceData)<-c("Open","High","Low","Close","Volume","Hold",'Settlement')


dygraph(Cl(priceData), main = "") %>%
  dyRangeSelector()
