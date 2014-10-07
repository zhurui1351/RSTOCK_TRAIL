library(blotter)
# initial currency instruments and load data
currency("USD")
stock("SPY",currency="USD",multiplier=1)
ls(envir=FinancialInstrument:::.instrument)
get("USD",envir=FinancialInstrument:::.instrument)
Sys.setenv(TZ="UTC")
startDate <- '1998-01-01'
endDate <- '2013-07-31'
#getSymbols('XLE', from=startDate, to=endDate, index.class=c("POSIXt","POSIXct"),adjust=T)
SPY=stockdata
SPY=to.monthly(SPY, indexAt='firstof', drop.time=FALSE)
SPY$SMA10m <- SMA(Cl(SPY), 10)
colnames(SPY)<-c("Open","High","Low","Close","Volume","SMA10m")
tail(SPY,3)
SPY=try.xts(SPY)
# initial portfolio and account
b.strategy <- "bFaber"
require(quantstrat)
rm.strat(b.strategy)
initPortf(b.strategy, 'SPY', initDate='1997-12-31')
#Note that initDate is prior to the start of the data
initAcct(b.strategy, portfolios=b.strategy, initDate='1997-12-31', initEq=1e6)
first(SPY)
ls(.blotter)
ls(envir=FinancialInstrument:::.instrument)

### bar by bar processing
# check price and indicatior to see if buy or sell is triggered
# update posion and equity

#Buy-Sell rules:
 # buy when monthly price > 10-month SMA
#sell and move to cash when monthly price < 10-month SMA

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
  if(!is.na(MA) ) # if the moving average has begun
  {
    if( Posn == 0 ) { # No position, test to go Long 
      if( ClosePrice > MA ) {
      # enter long position
      addTxn(b.strategy, Symbol='SPY', TxnDate=CurrentDate,
             TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0) } } else {
               # Have a position, so check exit
               if( ClosePrice < MA ) {
                 # exit position
                 addTxn(b.strategy, Symbol='SPY', TxnDate=CurrentDate,
                        TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0) } }
}
# Calculate P&L and resulting equity with blotter 
updatePortf(b.strategy, Dates = CurrentDate) 
updateAcct(b.strategy, Dates = CurrentDate)
updateEndEq(b.strategy, Dates = CurrentDate)
}

# analysis and reporting
chart.Posn(b.strategy, Symbol = 'SPY', Dates = '1998::',theme=myTheme)
tstats <- tradeStats(Portfolio=b.strategy, Symbol="SPY")

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

# performance analysis
library(PerformanceAnalytics)
rets <- PortfReturns(Account=b.strategy)
rownames(rets) <- NULL
tail(rets)
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

performance.stats.tab <- data.frame(
  rownames(tab.perf),tab.perf[,1],
  rownames(tab.risk),tab.risk[,1])

# buy and hold strategy
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
rets.fab <- Return.calculate(getAccount(b.strategy)$summary$End.Eq)
rets.bh <- Return.calculate(getAccount("buyHold")$summary$End.Eq)
returns <- cbind(rets.fab,rets.bh)
colnames(returns) <- c("Faber","BuyHold")
returns["2011"]
charts.PerformanceSummary(returns, geometric=FALSE, wealth.index=TRUE)
table.AnnualizedReturns(returns)
chart.RiskReturnScatter(returns, Rf = 0, add.sharpe = c(1, 2), xlim=c(0,0.25),
                        main = "Return versus Risk", colorset = c("red","blue"))
table.Stats(returns)
chart.RelativePerformance(returns[,1],returns[,2],
                          colorset = c("red","blue"), lwd = 2, legend.loc = "topleft")

# data struct
thePortfolio = getPortfolio(b.strategy)
names(thePortfolio)
names(thePortfolio$symbols)
names(thePortfolio$symbols$SPY)
names(thePortfolio$summary)
thePortfolio$symbols$SPY$txn
library(lattice)
xyplot(thePortfolio$symbols$SPY$posPL.USD,type="h",col=4)
xyplot(thePortfolio$summary,type="h",col=4)
str(thePortfolio)
theAccount = getAccount(b.strategy)
names(theAccount)
names(theAccount$portfolios)
names(theAccount$portfolios$bFaber)
names(theAccount$summary)

