ibrary('quantmod')
library('blotter')
rm(list=ls())
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

#导入源码包
sourceDir("C:/mac/desk/R/code/trade/SNPACKAGE/R",encoding='utf8')
#引入数据
path = "D:/stock/FOREX"
files = c('SH000001.TXT')
f = files[1]
fname <- file.path(path,f)
priceData <- read.zoo(fname,header=TRUE, format ="%Y-%m-%d",sep="\t",index.column=1)
colnames(priceData) <- c("Open","High","Low","Close","Volume","Amount")
#设置时间格式
index(priceData) <- as.POSIXct(index(priceData))
priceData = as.xts(priceData)
priceData = priceData['2000/2013']

###################################################
#以上证指数，周三做多为例进行测试 

result = list()
#优化atr参数n
for(n in 1 : 30)
{
  tradeRule = backTestBySnRule(rep(0,5),priceData,buyday=3,sellday=3,short = F,verbose = F,tradeDays = 5,type='iw',computePosition= volatilityPositionForTotalEq(priceData,n=n,ratio=0.01))
  result[[n]] = tradeRule
}

#优化固定头寸的size
result = list()
n = 100
#优化atr参数n
for(i in 1 : 20)
{
  n = i * 100
  tradeRule = backTestBySnRule(rep(0,5),priceData,buyday=3,sellday=3,short = F,verbose = F,tradeDays = 5,type='iw',computePosition= fixPosition(size = n))
  result[[i]] = tradeRule
}
