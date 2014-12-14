library('quantmod')
library('blotter')
#清除环境变量
rm("AUDJSP","strategy","account","initEq","portfolio")
rm("portfolio.AUDJSP","account.AUDJSP",pos=.blotter)

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
#导入源码包
sourceDir("C:/mac/desk/R/code/trade/SNPACKAGE/R",encoding='utf8')

path = "D:/stock/FOREX"
files <- dir(path)
files = c('AUDJAP.txt')
f = files[1]
fname <- file.path(path,f)

try(AUDJSP <- read.zoo(fname,header=TRUE, format ="%Y-%m-%d",sep="\t",index.column=1),TRUE) 

colnames(AUDJSP) <- c("Open","High","Low","Close","Volume","Amount")
#设置时间格式
index(AUDJSP) <- as.POSIXct(index(AUDJSP))
AUDJSP = as.xts(AUDJSP)
AUDJSP = initialData(AUDJSP)

#初始化货币
currency("USD")
#初始化证券
stock("AUDJSP", currency = "USD", multiplier = 1)
#设立时区
Sys.setenv(TZ = "UTC") 
#初始化组合和账户
strategy <- "AUDJSP"
initPortf(strategy, "AUDJSP", initDate = "2000-01-01")
initAcct(strategy, portfolios = strategy, initDate = "2000-01-01", initEq = 1e+06) #初始的资金是1，000，000

#交易日
tradeDays = getTradeInfoInByRule(c(0,0,0,0,0),AUDJSP,5,5)
#逐个数据处理
for( i in 1:nrow(AUDJSP) )
{
  #获取当前交易日期
  CurrentDate <- time(AUDJSP)[i]
  #获取最近的头寸
  equity<-getEndEq(strategy, CurrentDate)
  #当前仓位
  Posn <- getPosQty(strategy, Symbol='AUDJSP', Date=CurrentDate)
  
  
  
  #全仓交易
  UnitSize <-as.numeric(trunc(equity/ClosePrice))
  #获取当前收盘价
  ClosePrice <- as.numeric(Cl(AUDJSP[i,]))  
  #更新账户、头寸信息
  updatePortf(strategy, Dates = CurrentDate)
  updateAcct(strategy, Dates = CurrentDate)
  updateEndEq(strategy, Dates = CurrentDate)
  
}
#获取最终资金额
endeq=getEndEq("AUDJSP",Sys.time())
#绘制资金曲线图
chart.Posn(Portfolio='AUDJSP',Symbol='AUDJSP')
#获得总的交易信息，如笔数、胜负率等
states<-tradeStats("AUDJSP","AUDJSP")
print(states)
#获取每笔交易的信息
perstates<-perTradeStats('AUDJSP','AUDJSP')
print(perstates)
