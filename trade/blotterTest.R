currency("RMB")
stock("ZSYH", currency = "RMB", multiplier = 1)
ls(envir = FinancialInstrument:::.instrument) #设置 .instrument环境
get("RMB", envir = FinancialInstrument:::.instrument)
get("ZSYH", envir = FinancialInstrument:::.instrument)
Sys.setenv(TZ = "UTC") #设立时区
ZSYH <- getSymbols("600036.ss", from = "2008-01-01", to = Sys.Date(), src = "yahoo", 
                   auto.assign = FALSE)

ZSYH <- to.monthly(ZSYH, indexAt = "endof")
ZSYH$SMA10m <- SMA(Cl(ZSYH), 10)
head(ZSYH$SMA10m)

myTheme <- chart_theme()
myTheme$col$dn.col <- "lightgreen"
myTheme$col$up.col <- "lightblue"
myTheme$col$dn.border <- "grey"
myTheme$col$up.border <- "grey"
# plot OHLC series
chart_Series(x = ZSYH, theme = myTheme, name = "ZSYH", TA = "add_SMA(n=10,col=4)")
b.strategy <- "bFaber"
initPortf(b.strategy, "ZSYH", initDate = "2007-12-31")
initAcct(b.strategy, portfolios = b.strategy, initDate = "2007-12-31", initEq = 1e+06) #初始的资金是1e6，即1，000，000
ls(envir = FinancialInstrument:::.instrument)

for( i in 1:nrow(ZSYH) )
{
  #对日期更新
  CurrentDate <- time(ZSYH)[i]
  equity<-getEndEq(b.strategy, CurrentDate)
  ClosePrice <- as.numeric(Cl(ZSYH[i,]))
  Posn <- getPosQty(b.strategy, Symbol='ZSYH', Date=CurrentDate)
  UnitSize <-as.numeric(trunc(equity/ClosePrice))#全仓
  MA <- as.numeric(ZSYH[i,'SMA10m'])
  #如有必要改变头寸
  if(!is.na(MA)) #如果移动均线开始
  {
    if( Posn == 0 ) {#没有头寸，测试是否买入
      if( ClosePrice > MA ) {
        #进入多头头寸（买入）
        addTxn(b.strategy, Symbol='ZSYH', TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0) }
    } else {#有头寸，检测是否退出
      if( ClosePrice < MA ) {
        #退出头寸
        addTxn(b.strategy, Symbol='ZSYH', TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0) }
    }
  }
  #计算盈亏并更新
  updatePortf(b.strategy, Dates = CurrentDate)
  updateAcct(b.strategy, Dates = CurrentDate)
  updateEndEq(b.strategy, Dates = CurrentDate)
}
chart.Posn(b.strategy, Symbol = "ZSYH", Dates = "2008::", theme = myTheme)
getTxns(Portfolio = b.strategy, Symbol = "ZSYH")
(tstats <- tradeStats(Portfolio = b.strategy, Symbol = "ZSYH"))

library(PerformanceAnalytics)
rets <- PortfReturns(Account = b.strategy)
rownames(rets) <- NULL
charts.PerformanceSummary(rets, colorset = redfocus)

tab.perf <- table.Arbitrary(rets, metrics = c("Return.annualized", "SharpeRatio.annualized"), 
                            metricsNames = c("Annualized Return", "Annualized Sharpe Ratio"))
tab.perf
