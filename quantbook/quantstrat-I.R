### R code from vignette source 'quantstrat-I.Rnw'

###################################################
### code chunk number 1: "House keeping"
###################################################
options(width=82,continue=" ")
suppressWarnings(try(rm(list=c("filename")),silent=TRUE))


###################################################
### code chunk number 2: quantstrat-I.Rnw:201-203
###################################################
library(blotter)
search()


###################################################
### code chunk number 3: quantstrat-I.Rnw:230-232
###################################################
ls()
ls(all=T)


###################################################
### code chunk number 4: quantstrat-I.Rnw:268-270
###################################################
args(currency)
args(stock)


###################################################
### code chunk number 5: quantstrat-I.Rnw:285-287
###################################################
suppressWarnings(try(rm(list=c("account.bFaber","portfolio.bFaber"),pos=.blotter),silent=TRUE))
suppressWarnings(try(rm(list=c("b.strategy","myTheme","SPY",".getSymbols")),silent=TRUE))


###################################################
### code chunk number 6: quantstrat-I.Rnw:293-297
###################################################
currency("USD")
stock("SPY",currency="USD",multiplier=1)
ls(all=T)
ls(envir=FinancialInstrument:::.instrument)


###################################################
### code chunk number 7: quantstrat-I.Rnw:308-310
###################################################
get("USD",envir=FinancialInstrument:::.instrument)
get("SPY",envir=FinancialInstrument:::.instrument)


###################################################
### code chunk number 8: quantstrat-I.Rnw:318-323
###################################################
Sys.setenv(TZ="UTC")
startDate <- '1998-01-01'
endDate <- '2013-07-31'
getSymbols('SPY', from=startDate, to=endDate, index.class=c("POSIXt","POSIXct"), 
  adjust=T)


###################################################
### code chunk number 9: quantstrat-I.Rnw:325-328
###################################################
SPY=to.monthly(SPY, indexAt='endof', drop.time=FALSE)
SPY$SMA10m <- SMA(Cl(SPY), 10)
tail(SPY,3)


###################################################
### code chunk number 10: quantstrat-I.Rnw:348-349
###################################################
args(initPortf)


###################################################
### code chunk number 11: quantstrat-I.Rnw:366-367
###################################################
args(initAcct)


###################################################
### code chunk number 12: quantstrat-I.Rnw:383-389
###################################################
b.strategy <- "bFaber"
require(quantstrat)
rm.strat(b.strategy)
initPortf(b.strategy, 'SPY', initDate='1997-12-31')
initAcct(b.strategy, portfolios=b.strategy, initDate='1997-12-31', initEq=1e6)
first(SPY)


###################################################
### code chunk number 13: quantstrat-I.Rnw:400-403
###################################################
ls()
ls(.blotter)
ls(envir=FinancialInstrument:::.instrument)


###################################################
### code chunk number 14: FABER
###################################################
# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
# plot OHLC series
chart_Series(
  x=SPY,
  theme=myTheme,
  name="SPY",
  TA="add_SMA(n=10,col=4)"
  )


###################################################
### code chunk number 15: quantstrat-I.Rnw:471-500
###################################################
for( i in 1:nrow(SPY) )
{
  # update values for this date
  CurrentDate <- time(SPY)[i]
  equity = getEndEq(b.strategy, CurrentDate)
  ClosePrice <- as.numeric(Cl(SPY[i,]))
  Posn <- getPosQty(b.strategy, Symbol='SPY', Date=CurrentDate)
  UnitSize = as.numeric(trunc(equity/ClosePrice))
  MA <- as.numeric(SPY[i,'SMA10m'])
  # change market position if necessary
  if( !is.na(MA) ) # if the moving average has begun
  {
    if( Posn == 0 ) { # No position, test to go Long
      if( ClosePrice > MA ) {
        # enter long position
        addTxn(b.strategy, Symbol='SPY', TxnDate=CurrentDate,
          TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0) }
    } else { # Have a position, so check exit
      if( ClosePrice < MA ) {
        # exit position
        addTxn(b.strategy, Symbol='SPY', TxnDate=CurrentDate,
          TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0) }
    }
  }
  # Calculate P&L and resulting equity with blotter
  updatePortf(b.strategy, Dates = CurrentDate)
  updateAcct(b.strategy, Dates = CurrentDate)
  updateEndEq(b.strategy, Dates = CurrentDate)
} # End dates loop


###################################################
### code chunk number 16: quantstrat-I.Rnw:511-512
###################################################
args(updatePortf)


###################################################
### code chunk number 17: quantstrat-I.Rnw:527-528
###################################################
args(updateAcct)


###################################################
### code chunk number 18: quantstrat-I.Rnw:545-546
###################################################
args(updateEndEq)


###################################################
### code chunk number 19: quantstrat-I.Rnw:576-578
###################################################
args(chart.Posn)
chart.Posn(b.strategy, Symbol = 'SPY', Dates = '1998::',theme=myTheme)


###################################################
### code chunk number 20: quantstrat-I.Rnw:580-581
###################################################
pdf(file="plot-PERF.pdf",width=9,height=6)


###################################################
### code chunk number 21: quantstrat-I.Rnw:583-584
###################################################
plot(add_SMA(n=10,col=4, on=1))


###################################################
### code chunk number 22: quantstrat-I.Rnw:586-587
###################################################
dev.off()


###################################################
### code chunk number 23: quantstrat-I.Rnw:604-605
###################################################
options(width=110,continue=" ")


###################################################
### code chunk number 24: quantstrat-I.Rnw:607-608
###################################################
getTxns(Portfolio=b.strategy, Symbol="SPY")


###################################################
### code chunk number 25: quantstrat-I.Rnw:610-611
###################################################
options(width=82,continue=" ")


###################################################
### code chunk number 26: quantstrat-I.Rnw:622-623
###################################################
(tstats <- tradeStats(Portfolio=b.strategy, Symbol="SPY"))


###################################################
### code chunk number 27: quantstrat-I.Rnw:631-647
###################################################
# trade related
tab.trades <- cbind(
  c("Trades","Win Percent","Loss Percent","W/L Ratio"),
  c(tstats[,"Num.Trades"],tstats[,c("Percent.Positive","Percent.Negative")],
  tstats[,"Percent.Positive"]/tstats[,"Percent.Negative"]))
# profit related
tab.profit <- cbind(
  c("Net Profit","Gross Profits","Gross Losses","Profit Factor"),
  c(tstats[,c("Net.Trading.PL","Gross.Profits","Gross.Losses",
    "Profit.Factor")]))
# averages
tab.wins <- cbind(
  c("Avg Trade","Avg Win","Avg Loss","Avg W/L Ratio"),
  c(tstats[,c("Avg.Trade.PL","Avg.Win.Trade","Avg.Losing.Trade",
    "Avg.WinLoss.Ratio")]))
trade.stats.tab <- data.frame(tab.trades,tab.profit,tab.wins)


###################################################
### code chunk number 28: trade.stats.tab
###################################################
library(xtable)
print(xtable(trade.stats.tab, digits = c(0,0,2,0,2,0,2)),
  include.rownames = F,include.colnames=F, size="footnotesize")


###################################################
### code chunk number 29: PERF2
###################################################
library(PerformanceAnalytics)
rets <- PortfReturns(Account=b.strategy)
rownames(rets) <- NULL
tail(rets)
charts.PerformanceSummary(rets,colorset = bluefocus)


###################################################
### code chunk number 30: quantstrat-I.Rnw:714-715
###################################################
args(table.Arbitrary)


###################################################
### code chunk number 31: quantstrat-I.Rnw:733-745
###################################################
tab.perf <- table.Arbitrary(rets,
  metrics=c(
    "Return.cumulative",
    "Return.annualized",
    "SharpeRatio.annualized",
    "CalmarRatio"),
  metricsNames=c(
    "Cumulative Return",
    "Annualized Return",
    "Annualized Sharpe Ratio",
    "Calmar Ratio"))
tab.perf


###################################################
### code chunk number 32: quantstrat-I.Rnw:753-765
###################################################
tab.risk <- table.Arbitrary(rets,
  metrics=c(
    "StdDev.annualized",
    "maxDrawdown",
    "VaR",
    "ES"),
  metricsNames=c(
    "Annualized StdDev",
    "Max DrawDown",
    "Value-at-Risk",
    "Conditional VaR"))
tab.risk


###################################################
### code chunk number 33: quantstrat-I.Rnw:773-776
###################################################
performance.stats.tab <- data.frame(
  rownames(tab.perf),tab.perf[,1],
  rownames(tab.risk),tab.risk[,1])


###################################################
### code chunk number 34: performance.stats.tab
###################################################
print(xtable(performance.stats.tab, digits = c(0,0,3,0,3)),
  include.rownames = F,include.colnames=F, size="normalsize")


###################################################
### code chunk number 35: quantstrat-I.Rnw:794-811
###################################################
# initialize portfolio and account
rm.strat("buyHold")
initPortf("buyHold", 'SPY', initDate='1997-12-31')
initAcct("buyHold", portfolios="buyHold",
  initDate='1997-12-31', initEq=1e6)
# place a single transaction
CurrentDate <- as.Date(time(getTxns(Portfolio=b.strategy, Symbol="SPY"))[2])
equity = getEndEq("buyHold", CurrentDate)
ClosePrice <- as.numeric(Cl(SPY[CurrentDate,]))
UnitSize = as.numeric(trunc(equity/ClosePrice))
addTxn("buyHold", Symbol='SPY', TxnDate=CurrentDate, TxnPrice=ClosePrice,
  TxnQty = UnitSize , TxnFees=0)
# update portfolio and account
Dates <- paste(startDate,endDate,sep="::")
updatePortf(Portfolio="buyHold",Dates=Dates)
updateAcct(name="buyHold",Dates=Dates)
updateEndEq(Account="buyHold",Dates=Dates)


###################################################
### code chunk number 36: FABERBUYHOLDCOMP
###################################################
rets.fab <- Return.calculate(getAccount(b.strategy)$summary$End.Eq)
rets.bh <- Return.calculate(getAccount("buyHold")$summary$End.Eq)
returns <- cbind(rets.fab,rets.bh)
colnames(returns) <- c("Faber","BuyHold")
returns["2011"]
charts.PerformanceSummary(returns, geometric=FALSE, wealth.index=TRUE)


###################################################
### code chunk number 37: FBHRISKRETURN
###################################################
table.AnnualizedReturns(returns)
chart.RiskReturnScatter(returns, Rf = 0, add.sharpe = c(1, 2), xlim=c(0,0.25),
  main = "Return versus Risk", colorset = c("red","blue"))


###################################################
### code chunk number 38: FBHRELATIVE
###################################################
table.Stats(returns)
chart.RelativePerformance(returns[,1],returns[,2],
  colorset = c("red","blue"), lwd = 2, legend.loc = "topleft")


###################################################
### code chunk number 39: quantstrat-I.Rnw:882-887
###################################################
thePortfolio = getPortfolio(b.strategy)
names(thePortfolio)
names(thePortfolio$symbols)
names(thePortfolio$symbols$SPY)
names(thePortfolio$summary)


###################################################
### code chunk number 40: quantstrat-I.Rnw:904-905
###################################################
options(width=110,continue=" ")


###################################################
### code chunk number 41: quantstrat-I.Rnw:907-908
###################################################
thePortfolio$symbols$SPY$txn


###################################################
### code chunk number 42: quantstrat-I.Rnw:910-911
###################################################
options(width=82,continue=" ")


###################################################
### code chunk number 43: quantstrat-I.Rnw:932-933
###################################################
library(lattice)


###################################################
### code chunk number 44: quantstrat-I.Rnw:935-936
###################################################
pdf(file="plot-BLOTPOSPL.pdf",width=9,height=6)


###################################################
### code chunk number 45: quantstrat-I.Rnw:938-939
###################################################
xyplot(thePortfolio$symbols$SPY$posPL.USD,type="h",col=4)


###################################################
### code chunk number 46: quantstrat-I.Rnw:941-942
###################################################
dev.off()


###################################################
### code chunk number 47: quantstrat-I.Rnw:944-945
###################################################
pdf(file="plot-BLOTSUM.pdf",width=9,height=6)


###################################################
### code chunk number 48: quantstrat-I.Rnw:947-948
###################################################
xyplot(thePortfolio$summary,type="h",col=4)


###################################################
### code chunk number 49: quantstrat-I.Rnw:950-951
###################################################
dev.off()


###################################################
### code chunk number 50: quantstrat-I.Rnw:977-978
###################################################
args(str)


###################################################
### code chunk number 51: quantstrat-I.Rnw:992-993
###################################################
str(thePortfolio)


###################################################
### code chunk number 52: quantstrat-I.Rnw:1003-1008
###################################################
theAccount = getAccount(b.strategy)
names(theAccount)
names(theAccount$portfolios)
names(theAccount$portfolios$bFaber)
names(theAccount$summary)


