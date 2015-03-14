require(quantmod)
require(blotter)
require(compiler)
path = "C:/R"
f = 'AUDUSD.txt'
fname = file.path(path,f)

priceData <- read.table(fname,sep=',',header=T,colClasses = rep(c("NULL", "character", "numeric"), c(1, 2, 5)))

priceData <- read.zoo(priceData, sep=",", header=TRUE, 
                      index.column=1:2, format="%Y%m%d %H%M%S", tz="")
priceData <- as.xts(priceData)

backdata = priceData
priceData = priceData['2013']

colnames(priceData) <- c("Open","High","Low","Close","Volume")


RBreaker_cmp <- cmpfun(RBreaker)
dualthrust_cmp <-cmpfun(dualthrust)

RBreaker_cmp(priceData,minute = 60,verbose = F)
r_stat = tradeStats('m_data','m_data')
dualthrust_cmp(priceData,verbose=F,minute = 60)
d_stat = tradeStats('m_data','m_data')
