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

sourceDir("C:/mac/desk/R/code/trade/SNPACKAGE/R",encoding='utf8')
#导入源码包

path = "D:/stock/FOREX"
reportpath = "D:/myreport"
files <- dir(path)
files = c('USDCHF.TXT')
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
  reports = SnWeeklyReport(priceData,confProp=0.55,profitratio = 0.15,drawdownratio = -0.1,path=paste(reportpath,'/',f,sep=""))
  print(reports)
}


#手工测试单个rule
#snData = initialData(priceData)
#snrule1 = getBasePropByRules(list(c(-1,0,0,0,0),c(-1,0,0,1,0)),snData)
#snrule = getConditionRules(snrule1)

#tradeRule = backTestBySnRule(c(-1,0,0,0,0),priceData,buyday=4,sellday=4,short = FALSE)

#分析每笔交易
#txns = tradeRule$txns
#txns =txns[2:nrow(txns)]
#names(txns) = c('qty','price','fee','value','cost','net')
#txns=txns[,c(1,2,3,4,6)]
#拆分交易
#ti = 1 : nrow(txns)
#隔排拆分
#te = ti[ti%%2==0]
#to = ti[ti%%2==1]

#txne = txns[te]
#txno = txns[to]

# 按日期合并,形成每日交易明细
#txnall = merge.xts(txne,txno,join="inner")
#names(txnall) = c('sellqty','sellprice','sellfee','sellvalue','sellnet','buyqty','buyprice','buyfee','buyvalue','buynet')
#netprice = txnall$sellprice - txnall$buyprice

#length(netprice[netprice > 0])
