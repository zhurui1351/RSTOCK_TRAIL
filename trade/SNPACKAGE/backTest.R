library('quantmod')
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
#导入分析函数
source("C:/mac/desk/R/code/trade/SNPACKAGE/analysis/performanceInfo.R",encoding='utf8')
#运行一遍测试，一般用于代码修改以后
sourceDir("C:/mac/desk/R/code/trade/SNPACKAGE/test",encoding='utf8')


#运行整个sn找出规则
path = "D:/stock/FOREX"
reportpath = "D:/myreport_m"
files <- dir(path)
#files = c('SH000001.TXT')
#f = files[1]
#fname <- file.path(path,f)
for(f in files)
{
  fname = file.path(path,f)
  priceData <- read.zoo(fname,header=TRUE, format ="%Y-%m-%d",sep="\t",index.column=1)
  colnames(priceData) <- c("Open","High","Low","Close","Volume","Amount")
#设置时间格式
  index(priceData) <- as.POSIXct(index(priceData))
  priceData = as.xts(priceData)
  priceData = priceData['1994/2013']
#固定头寸1000
  reports = SnReport(priceData,confProp=0.6,profitratio = 0.2,prune=3,drawdownratio = -5,path=paste(reportpath,'/',f,sep=""),type='bm',tradeDays = 12,computePosition=fixPosition(size=1000))
# print(reports)
}


#手工测试单个rule
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
#snData = initialData(priceData,type='iw')
#monthrule=rep(0,53)
#monthrule[3] = 1
#monthrule[4] = -1

#snrule1 = getBasePropByRules(list(monthrule),snData,tradeDays = 12)
#snrule = getConditionRules(snrule1)

tradeRule = backTestBySnRule(rep(0,5),priceData,buyday=3,sellday=3,short = F,verbose = F,tradeDays = 5,type='iw',computePosition=volatilityPositionForTotalEq(priceData,n=3,ratio=0.01))
