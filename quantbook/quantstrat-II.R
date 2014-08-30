### R code from vignette source 'quantstrat-II.Rnw'

###################################################
### code chunk number 1: "House keeping"
###################################################
options(width=82,continue=" ")
suppressWarnings(try(rm(list=ls()),silent=TRUE))


###################################################
### code chunk number 2: quantstrat-II.Rnw:221-223
###################################################
library(quantstrat)
search()


###################################################
### code chunk number 3: quantstrat-II.Rnw:252-253
###################################################
suppressWarnings(try(rm(list=ls(FinancialInstrument:::.instrument),pos=FinancialInstrument:::.instrument),silent=TRUE))


###################################################
### code chunk number 4: quantstrat-II.Rnw:259-263
###################################################
currency("USD")
stock("SPY",currency="USD",multiplier=1)
ls(envir=FinancialInstrument:::.instrument)
ls(all=T)


###################################################
### code chunk number 5: quantstrat-II.Rnw:276-281
###################################################
# system settings
initDate <- '1997-12-31'
startDate <- '1998-01-01'
endDate <-  '2013-07-31'
initEq <- 1e6


###################################################
### code chunk number 6: quantstrat-II.Rnw:283-284
###################################################
Sys.setenv(TZ="UTC")


###################################################
### code chunk number 7: quantstrat-II.Rnw:286-293
###################################################
if(file.exists("SPY.RData"))
{
  load("SPY.RData")
} else {
  getSymbols('SPY', from=startDate, to=endDate, adjust=T)
  save(list="SPY",file="SPY.RData")
}


###################################################
### code chunk number 8: quantstrat-II.Rnw:295-296 (eval = FALSE)
###################################################
## getSymbols('SPY', from=startDate, to=endDate, adjust=T)


###################################################
### code chunk number 9: quantstrat-II.Rnw:298-300
###################################################
SPY=to.monthly(SPY, indexAt='endof')
SPY$SMA10m <- SMA(Cl(SPY), 10)


###################################################
### code chunk number 10: quantstrat-II.Rnw:318-320
###################################################
# inz portfolio, account
qs.strategy <- "qsFaber"


###################################################
### code chunk number 11: quantstrat-II.Rnw:322-323
###################################################
rm.strat(qs.strategy) # remove strategy etc. if this is a re-run


###################################################
### code chunk number 12: quantstrat-II.Rnw:325-326
###################################################
initPortf(qs.strategy,'SPY', initDate=initDate)


###################################################
### code chunk number 13: quantstrat-II.Rnw:328-329
###################################################
initAcct(qs.strategy,portfolios=qs.strategy, initDate=initDate, initEq=initEq)


###################################################
### code chunk number 14: quantstrat-II.Rnw:346-352
###################################################
# initialize orders container
args(initOrders)
initOrders(portfolio=qs.strategy,initDate=initDate)
# instantiate a new strategy object
args(strategy)
strategy(qs.strategy,store=TRUE)


###################################################
### code chunk number 15: quantstrat-II.Rnw:363-366
###################################################
ls(all=T)
ls(.blotter)
ls(.strategy)


###################################################
### code chunk number 16: quantstrat-II.Rnw:378-382
###################################################
args(getStrategy)
strat <-getStrategy(qs.strategy)
class(strat)
summary(strat)


###################################################
### code chunk number 17: quantstrat-II.Rnw:417-418
###################################################
args(add.indicator)


###################################################
### code chunk number 18: quantstrat-II.Rnw:436-438
###################################################
add.indicator(strategy = qs.strategy, name = "SMA",
  arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")


###################################################
### code chunk number 19: quantstrat-II.Rnw:440-441
###################################################
summary(getStrategy(qs.strategy))


###################################################
### code chunk number 20: quantstrat-II.Rnw:457-458
###################################################
args(add.signal)


###################################################
### code chunk number 21: quantstrat-II.Rnw:476-479
###################################################
add.signal(qs.strategy,name="sigCrossover",
  arguments = list(columns=c("Close","SMA10"),relationship="gt"),
  label="Cl.gt.SMA")


###################################################
### code chunk number 22: quantstrat-II.Rnw:481-484
###################################################
add.signal(qs.strategy,name="sigCrossover",
  arguments = list(columns=c("Close","SMA10"),relationship="lt"),
  label="Cl.lt.SMA")


###################################################
### code chunk number 23: quantstrat-II.Rnw:486-487
###################################################
summary(getStrategy(qs.strategy))


###################################################
### code chunk number 24: quantstrat-II.Rnw:496-497
###################################################
args(add.rule)


###################################################
### code chunk number 25: quantstrat-II.Rnw:513-514
###################################################
args(ruleSignal)


###################################################
### code chunk number 26: quantstrat-II.Rnw:535-540
###################################################
#   go long when close > MA
add.rule(qs.strategy, name='ruleSignal',
  arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=900,
  ordertype='market', orderside='long', pricemethod='market'),
  type='enter', path.dep=TRUE)


###################################################
### code chunk number 27: quantstrat-II.Rnw:542-547
###################################################
#   exit when close < MA
add.rule(qs.strategy, name='ruleSignal',
  arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
  ordertype='market', orderside='long', pricemethod='market'),
  type='exit', path.dep=TRUE)


###################################################
### code chunk number 28: quantstrat-II.Rnw:556-557
###################################################
summary(getStrategy(qs.strategy))


###################################################
### code chunk number 29: quantstrat-II.Rnw:579-580
###################################################
args(applyStrategy)


###################################################
### code chunk number 30: quantstrat-II.Rnw:594-595
###################################################
applyStrategy(strategy=qs.strategy , portfolios=qs.strategy)


###################################################
### code chunk number 31: quantstrat-II.Rnw:606-607
###################################################
options(width=110)


###################################################
### code chunk number 32: quantstrat-II.Rnw:609-610
###################################################
mktdata["199907/199912"]


###################################################
### code chunk number 33: quantstrat-II.Rnw:612-613
###################################################
options(width=82)


###################################################
### code chunk number 34: quantstrat-II.Rnw:640-641
###################################################
Dates <- paste(startDate,endDate,sep="::")


###################################################
### code chunk number 35: quantstrat-II.Rnw:643-644
###################################################
updatePortf(qs.strategy,Dates=Dates)


###################################################
### code chunk number 36: quantstrat-II.Rnw:646-647
###################################################
updateAcct(qs.strategy,Dates=Dates)


###################################################
### code chunk number 37: quantstrat-II.Rnw:649-650
###################################################
updateEndEq(qs.strategy,Dates=Dates)


###################################################
### code chunk number 38: quantstrat-II.Rnw:668-696
###################################################
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}
checkBlotterUpdate(qs.strategy,qs.strategy)


###################################################
### code chunk number 39: quantstrat-II.Rnw:719-724
###################################################
# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'


###################################################
### code chunk number 40: quantstrat-II.Rnw:726-728
###################################################
# plot performance
chart.Posn(qs.strategy, Symbol = 'SPY', Dates = '1998::',theme=myTheme)


###################################################
### code chunk number 41: quantstrat-II.Rnw:730-731
###################################################
pdf(file="plot-PERFQS.pdf",width=9,height=6)


###################################################
### code chunk number 42: quantstrat-II.Rnw:733-734
###################################################
plot(add_SMA(n=10,col=4, on=1, lwd=2))


###################################################
### code chunk number 43: quantstrat-II.Rnw:736-737
###################################################
dev.off()


###################################################
### code chunk number 44: quantstrat-II.Rnw:752-753
###################################################
getTxns(Portfolio=qs.strategy, Symbol="SPY")


###################################################
### code chunk number 45: quantstrat-II.Rnw:762-763
###################################################
(tstats <- tradeStats(Portfolio=qs.strategy, Symbol="SPY"))


###################################################
### code chunk number 46: quantstrat-II.Rnw:772-777
###################################################
ob <- getOrderBook(qs.strategy)
class(ob)
names(ob)
names(ob$qsFaber)
names(ob$qsFaber$SPY)


###################################################
### code chunk number 47: quantstrat-II.Rnw:787-788
###################################################
options(width=110)


###################################################
### code chunk number 48: quantstrat-II.Rnw:790-791
###################################################
ob$qsFaber$SPY[,1:5]


###################################################
### code chunk number 49: quantstrat-II.Rnw:793-794
###################################################
options(width=82)


###################################################
### code chunk number 50: quantstrat-II.Rnw:806-807
###################################################
options(width=110)


###################################################
### code chunk number 51: quantstrat-II.Rnw:809-810
###################################################
ob$qsFaber$SPY[,6:11]


###################################################
### code chunk number 52: quantstrat-II.Rnw:812-813
###################################################
options(width=82)


###################################################
### code chunk number 53: quantstrat-II.Rnw:825-826
###################################################
options(width=100)


###################################################
### code chunk number 54: quantstrat-II.Rnw:828-829
###################################################
perTradeStats(qs.strategy)


###################################################
### code chunk number 55: quantstrat-II.Rnw:831-832
###################################################
options(width=82)


###################################################
### code chunk number 56: quantstrat-II.Rnw:843-844
###################################################
pdf(file="plot-FABERMAE.pdf",width=5,height=5)


###################################################
### code chunk number 57: quantstrat-II.Rnw:846-847
###################################################
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MAE', scale='percent')


###################################################
### code chunk number 58: quantstrat-II.Rnw:849-850
###################################################
dev.off()


###################################################
### code chunk number 59: quantstrat-II.Rnw:852-853
###################################################
pdf(file="plot-FABERMFE.pdf",width=5,height=5)


###################################################
### code chunk number 60: quantstrat-II.Rnw:855-856
###################################################
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MFE', scale='percent')


###################################################
### code chunk number 61: quantstrat-II.Rnw:858-859
###################################################
dev.off()


###################################################
### code chunk number 62: quantstrat-II.Rnw:884-885
###################################################
options(width=110)


###################################################
### code chunk number 63: ACCTSUM
###################################################
a <- getAccount(qs.strategy)
last(a$summary,5)
library(lattice)
xyplot(a$summary,type="h",col=4)


###################################################
### code chunk number 64: quantstrat-II.Rnw:893-894
###################################################
options(width=82)


###################################################
### code chunk number 65: quantstrat-II.Rnw:915-916
###################################################
equity <- a$summary$End.Eq


###################################################
### code chunk number 66: quantstrat-II.Rnw:918-919
###################################################
pdf(file="plot-EQCURVE.pdf",width=9,height=6)


###################################################
### code chunk number 67: quantstrat-II.Rnw:921-922
###################################################
plot(equity,main="Faber Strategy Equity Curve")


###################################################
### code chunk number 68: quantstrat-II.Rnw:924-925
###################################################
dev.off()


###################################################
### code chunk number 69: quantstrat-II.Rnw:927-928
###################################################
ret <- Return.calculate(equity,method="log")


###################################################
### code chunk number 70: quantstrat-II.Rnw:930-931
###################################################
pdf(file="plot-PERFSUM.pdf",width=9,height=6)


###################################################
### code chunk number 71: quantstrat-II.Rnw:933-935
###################################################
charts.PerformanceSummary(ret, colorset = bluefocus,
  main="Faber Strategy Performance")


###################################################
### code chunk number 72: quantstrat-II.Rnw:937-938
###################################################
dev.off()


###################################################
### code chunk number 73: quantstrat-II.Rnw:992-993
###################################################
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")


###################################################
### code chunk number 74: quantstrat-II.Rnw:995-1002
###################################################
if(file.exists("XLX.RData"))
{
  load("XLX.RData")
} else {
  getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from=startDate, to=endDate, adjust=T)
  save(list=symbols,file="XLX.RData")
}


###################################################
### code chunk number 75: quantstrat-II.Rnw:1004-1006 (eval = FALSE)
###################################################
## getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
##   from=startDate, to=endDate, adjust=T)


###################################################
### code chunk number 76: quantstrat-II.Rnw:1008-1017
###################################################
for(symbol in symbols)
{
    stock(symbol, currency="USD",multiplier=1)
    x<-get(symbol)
    x<-to.monthly(x,indexAt='lastof',drop.time=TRUE)
    indexFormat(x)<-'%Y-%m-%d'
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
}


###################################################
### code chunk number 77: quantstrat-II.Rnw:1028-1030
###################################################
multi.asset <- "multiAsset"
rm.strat(multi.asset) # remove strategy etc. if this is a re-run


###################################################
### code chunk number 78: quantstrat-II.Rnw:1032-1036
###################################################
initPortf(multi.asset,symbols=symbols, initDate=initDate)
initAcct(multi.asset,portfolios=multi.asset, initDate=initDate,
  initEq=initEq)
initOrders(portfolio=multi.asset,initDate=initDate)


###################################################
### code chunk number 79: quantstrat-II.Rnw:1047-1048
###################################################
out <- applyStrategy(strategy=qs.strategy , portfolios=multi.asset)


###################################################
### code chunk number 80: quantstrat-II.Rnw:1050-1054
###################################################
Dates <- paste(startDate,endDate,sep="::")
updatePortf(multi.asset,Dates=Dates)
updateAcct(multi.asset,Dates=Dates)
updateEndEq(multi.asset,Dates=Dates)


###################################################
### code chunk number 81: quantstrat-II.Rnw:1056-1057
###################################################
checkBlotterUpdate(multi.asset,multi.asset)


###################################################
### code chunk number 82: quantstrat-II.Rnw:1068-1071
###################################################
a <- getAccount(multi.asset)
p <- getPortfolio(multi.asset)
names(p$symbols)


###################################################
### code chunk number 83: quantstrat-II.Rnw:1085-1086
###################################################
pdf(file="plot-XLX3x3.pdf",width=18,height=12,pointsize=8)


###################################################
### code chunk number 84: quantstrat-II.Rnw:1088-1095
###################################################
par(mfrow=c(3,3))
for(symbol in symbols)
{
  chart.Posn(Portfolio=multi.asset,Symbol=symbol,theme=myTheme,
    TA="add_SMA(n=10,col='blue')")
}
par(mfrow=c(1,1))


###################################################
### code chunk number 85: quantstrat-II.Rnw:1097-1098
###################################################
dev.off()


###################################################
### code chunk number 86: quantstrat-II.Rnw:1115-1116
###################################################
options(width=120)


###################################################
### code chunk number 87: quantstrat-II.Rnw:1118-1119
###################################################
tradeStats(multi.asset)


###################################################
### code chunk number 88: quantstrat-II.Rnw:1121-1122
###################################################
options(width=82)


###################################################
### code chunk number 89: IASSRET
###################################################
rets.multi <- PortfReturns(multi.asset)
colnames(rets.multi) <- symbols
rets.multi$TOTAL<-rowSums(rets.multi)
rets.multi <- rets.multi[,c("TOTAL",symbols)]
round(tail(rets.multi,5),6)
chart.CumReturns(rets.multi, colorset= rich10equal, legend.loc = "topleft",
  main="SPDR Cumulative Returns")


###################################################
### code chunk number 90: SPDRBOX
###################################################
chart.Boxplot(rets.multi, main = "SPDR Returns", colorset= rich10equal)


###################################################
### code chunk number 91: MULTIRETRISK
###################################################
(ar.tab <- table.AnnualizedReturns(rets.multi))
max.risk <- max(ar.tab["Annualized Std Dev",])
max.return <- max(ar.tab["Annualized Return",])

chart.RiskReturnScatter(rets.multi,
  main = "SPDR Performance", colorset = rich10equal,
  xlim=c(0,max.risk*1.1),ylim=c(0,max.return))


###################################################
### code chunk number 92: quantstrat-II.Rnw:1187-1188
###################################################
equity <- a$summary$End.Eq


###################################################
### code chunk number 93: quantstrat-II.Rnw:1190-1191
###################################################
pdf(file="plot-MULTIEQCURVE.pdf",width=9,height=6)


###################################################
### code chunk number 94: quantstrat-II.Rnw:1193-1194
###################################################
plot(equity,main="Consolidated SPDR Equity Curve")


###################################################
### code chunk number 95: quantstrat-II.Rnw:1196-1197
###################################################
dev.off()


###################################################
### code chunk number 96: quantstrat-II.Rnw:1199-1200
###################################################
pdf(file="plot-MULTIPERFORMANCE.pdf",width=9,height=6)


###################################################
### code chunk number 97: quantstrat-II.Rnw:1202-1204
###################################################
charts.PerformanceSummary(Return.calculate(equity), geometric=FALSE, 
  wealth.index=TRUE, main="Consolidated SPDR Performance")


###################################################
### code chunk number 98: quantstrat-II.Rnw:1206-1207
###################################################
dev.off()


