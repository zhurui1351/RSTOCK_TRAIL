### R code from vignette source 'quantstrat-IV.Rnw'

###################################################
### code chunk number 1: "House keeping"
###################################################
options(width=82,continue=" ")
suppressWarnings(try(rm(list=ls()),silent=TRUE))


###################################################
### code chunk number 2: quantstrat-IV.Rnw:214-216
###################################################
Sys.setenv(TZ="UTC")
library(quantstrat)


###################################################
### code chunk number 3: quantstrat-IV.Rnw:218-221
###################################################
initDate = '2002-10-21'
.from=initDate
.to='2002-10-31'


###################################################
### code chunk number 4: quantstrat-IV.Rnw:223-225
###################################################
currency(c('GBP', 'USD'))
exchange_rate('GBPUSD', tick_size=0.0001)


###################################################
### code chunk number 5: quantstrat-IV.Rnw:227-233
###################################################
getSymbols.FI(Symbols='GBPUSD',
	      dir=system.file('extdata',package='quantstrat'),
	      from=.from, to=.to)

GBPUSD = to.minutes30(GBPUSD)
GBPUSD = align.time(GBPUSD, 1800)


###################################################
### code chunk number 6: quantstrat-IV.Rnw:244-246
###################################################
dim(GBPUSD)
last(GBPUSD,5)


###################################################
### code chunk number 7: GBPUSD
###################################################
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chart_Series(GBPUSD,theme=myTheme)


###################################################
### code chunk number 8: quantstrat-IV.Rnw:268-271
###################################################
# moving average lengths
.fast = 10
.slow = 30


###################################################
### code chunk number 9: quantstrat-IV.Rnw:273-276
###################################################
# optimization range
.FastSMA = (1:20)
.SlowSMA = (30:80)


###################################################
### code chunk number 10: quantstrat-IV.Rnw:278-282
###################################################
# trade parameters
.threshold = 0.0005
.orderqty = 100000
.txnfees = -6  # round-trip fee


###################################################
### code chunk number 11: quantstrat-IV.Rnw:284-287
###################################################
# stop loss amount
.stoploss <- 0.30/100
.StopLoss = seq(0.05, 0.6, length.out=48)/100


###################################################
### code chunk number 12: quantstrat-IV.Rnw:289-291
###################################################
# trading window
.timespan = 'T00:00/T23:59'


###################################################
### code chunk number 13: quantstrat-IV.Rnw:293-295
###################################################
# number of optimization samples
.nsamples=80


###################################################
### code chunk number 14: quantstrat-IV.Rnw:303-306
###################################################
portfolio.st = 'forex'
account.st = 'IB1'
strategy.st = 'luxor'


###################################################
### code chunk number 15: quantstrat-IV.Rnw:308-310
###################################################
rm.strat(portfolio.st)
rm.strat(account.st)


###################################################
### code chunk number 16: quantstrat-IV.Rnw:312-316
###################################################
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)


###################################################
### code chunk number 17: quantstrat-IV.Rnw:324-331
###################################################
add.indicator(strategy.st, name = "SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .fast
	),
	label="nFast"
)


###################################################
### code chunk number 18: quantstrat-IV.Rnw:333-340
###################################################
add.indicator(strategy.st, name="SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .slow
	),
	label="nSlow"
)


###################################################
### code chunk number 19: quantstrat-IV.Rnw:353-360
###################################################
add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="gte"
	),
	label='long'
)


###################################################
### code chunk number 20: quantstrat-IV.Rnw:362-369
###################################################
add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="lt"
	),
	label='short'
)


###################################################
### code chunk number 21: quantstrat-IV.Rnw:380-392
###################################################
add.rule(strategy.st, name='ruleSignal',
    arguments=list(sigcol='long' , sigval=TRUE,
        orderside='long' ,
        ordertype='stoplimit',
        prefer='High',
        threshold=.threshold,
        orderqty=+.orderqty,
        replace=FALSE
        ),
    type='enter',
    label='EnterLONG'
)


###################################################
### code chunk number 22: quantstrat-IV.Rnw:406-418
###################################################
add.rule(strategy.st, name='ruleSignal',
    arguments=list(sigcol='short', sigval=TRUE,
        orderside='short',
        ordertype='stoplimit',
        prefer='Low',
        threshold=-.threshold,
        orderqty=-.orderqty,
        replace=FALSE
        ),
    type='enter',
    label='EnterSHORT'
)


###################################################
### code chunk number 23: quantstrat-IV.Rnw:432-443
###################################################
add.rule(strategy.st, name='ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		orderside='short',
		ordertype='market',
		orderqty='all',
		TxnFees=.txnfees,
		replace=TRUE
	),
	type='exit',
	label='Exit2LONG'
)


###################################################
### code chunk number 24: quantstrat-IV.Rnw:455-466
###################################################
add.rule(strategy.st, name='ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		orderside='long' ,
		ordertype='market',
		orderqty='all',
		TxnFees=.txnfees,
		replace=TRUE
	),
	type='exit',
	label='Exit2SHORT'
)


###################################################
### code chunk number 25: quantstrat-IV.Rnw:478-481
###################################################
out <- applyStrategy(strategy.st, portfolio.st)
updatePortf(portfolio.st, Symbols='GBPUSD',
  Dates=paste('::',as.Date(Sys.time()),sep=''))


###################################################
### code chunk number 26: CHARTLUXOR1
###################################################
chart.Posn(portfolio.st, "GBPUSD",
  TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)",theme=myTheme)


###################################################
### code chunk number 27: quantstrat-IV.Rnw:487-488
###################################################
pdf(file="plot-LUXORSTATS1.pdf",height=8.5,width=5.5)


###################################################
### code chunk number 28: quantstrat-IV.Rnw:490-491
###################################################
PerformanceAnalytics:::textplot(t(tradeStats(portfolio.st, 'GBPUSD')))


###################################################
### code chunk number 29: quantstrat-IV.Rnw:493-494
###################################################
dev.off()


###################################################
### code chunk number 30: quantstrat-IV.Rnw:496-497
###################################################
pdf(file="plot-MKTDATA.pdf",height=6,width=9)


###################################################
### code chunk number 31: quantstrat-IV.Rnw:499-502
###################################################
mk <- mktdata['2002-10-23 15:00::2002-10-24 03:00']
mk.df <- data.frame(Date=time(mk),coredata(mk))
PerformanceAnalytics:::textplot(mk.df,show.rownames=F)


###################################################
### code chunk number 32: quantstrat-IV.Rnw:504-505
###################################################
dev.off()


###################################################
### code chunk number 33: quantstrat-IV.Rnw:507-508
###################################################
pdf(file="plot-ORDERBOOK.pdf",height=6,width=9)


###################################################
### code chunk number 34: quantstrat-IV.Rnw:510-513
###################################################
ob <- getOrderBook(portfolio.st)$forex$GBPUSD
ob.df <- data.frame(Date=time(ob),coredata(ob))
PerformanceAnalytics:::textplot(ob.df,show.rownames=F)


###################################################
### code chunk number 35: quantstrat-IV.Rnw:515-516
###################################################
dev.off()


###################################################
### code chunk number 36: quantstrat-IV.Rnw:518-519
###################################################
pdf(file="plot-PERTRADESTATS.pdf",height=5,width=7)


###################################################
### code chunk number 37: quantstrat-IV.Rnw:521-523
###################################################
PerformanceAnalytics:::textplot(perTradeStats(portfolio.st,"GBPUSD"),
  show.rownames=F)


###################################################
### code chunk number 38: quantstrat-IV.Rnw:525-526
###################################################
dev.off()


###################################################
### code chunk number 39: quantstrat-IV.Rnw:528-529
###################################################
pdf(file="plot-MAE.pdf",height=5,width=5)


###################################################
### code chunk number 40: quantstrat-IV.Rnw:531-532
###################################################
chart.ME(portfolio.st,'GBPUSD',type='MAE',scale='percent')


###################################################
### code chunk number 41: quantstrat-IV.Rnw:534-535
###################################################
dev.off()


###################################################
### code chunk number 42: quantstrat-IV.Rnw:611-612
###################################################
args(add.distribution)


###################################################
### code chunk number 43: quantstrat-IV.Rnw:614-621
###################################################
add.distribution(strategy.st,
	paramset.label = 'SMA',
	component.type = 'indicator',
	component.label = 'nFast',
	variable = list(n = .FastSMA),
	label = 'nFAST'
)


###################################################
### code chunk number 44: quantstrat-IV.Rnw:633-640
###################################################
add.distribution(strategy.st,
	paramset.label = 'SMA',
	component.type = 'indicator',
	component.label = 'nSlow',
	variable = list(n = .SlowSMA),
	label = 'nSLOW'
)


###################################################
### code chunk number 45: quantstrat-IV.Rnw:652-653
###################################################
args(add.constraint)


###################################################
### code chunk number 46: quantstrat-IV.Rnw:655-662
###################################################
add.distribution.constraint(strategy.st,
	paramset.label = 'SMA',
	distribution.label.1 = 'nFAST',
	distribution.label.2 = 'nSLOW',
	operator = '<',
	label = 'SMA'
)


###################################################
### code chunk number 47: quantstrat-IV.Rnw:673-675
###################################################
rm.strat(portfolio.st)
rm.strat(account.st)


###################################################
### code chunk number 48: quantstrat-IV.Rnw:677-681
###################################################
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,
  initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)


###################################################
### code chunk number 49: quantstrat-IV.Rnw:704-706
###################################################
library(parallel)
detectCores()


###################################################
### code chunk number 50: quantstrat-IV.Rnw:708-716
###################################################
if( Sys.info()['sysname'] == "Windows" )
{
  library(doParallel)
  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}


###################################################
### code chunk number 51: quantstrat-IV.Rnw:718-719
###################################################
foreach(i=1:8, .combine=c) %dopar% sqrt(i)


###################################################
### code chunk number 52: quantstrat-IV.Rnw:721-725
###################################################
if( Sys.info()['sysname'] == "Windows" )
{
  registerDoSEQ()
}


###################################################
### code chunk number 53: quantstrat-IV.Rnw:727-727
###################################################



###################################################
### code chunk number 54: quantstrat-IV.Rnw:739-740
###################################################
args(apply.paramset)


###################################################
### code chunk number 55: quantstrat-IV.Rnw:755-774
###################################################
if( Sys.info()['sysname'] == "Windows" )
{
  if(file.exists("resultsMAOpt.RData"))
  {
    load("resultsMAOpt.RData")
  } else {
    results <- apply.paramset(strategy.st, paramset.label='SMA',
      portfolio.st=portfolio.st, account.st=account.st, nsamples=.nsamples)
  }
} else {
  if(file.exists("resultsMAOpt.RData"))
  {
    load("resultsMAOpt.RData")
  } else {
    results <- apply.paramset(strategy.st, paramset.label='SMA',
      portfolio.st=portfolio.st, account.st=account.st, nsamples=0)
    save(list="results",file="resultsMAOpt.RData")
  }
}


###################################################
### code chunk number 56: quantstrat-IV.Rnw:776-778 (eval = FALSE)
###################################################
## results <- apply.paramset(strategy.st, paramset.label='SMA',
##   portfolio.st=portfolio.st, account.st=account.st, nsamples=.nsamples)


###################################################
### code chunk number 57: quantstrat-IV.Rnw:780-781
###################################################
head(names(results),20)


###################################################
### code chunk number 58: quantstrat-IV.Rnw:783-784
###################################################
pdf(file="plot-PARAMSETTRADESTATS.pdf",height=6,width=9)


###################################################
### code chunk number 59: quantstrat-IV.Rnw:786-790
###################################################
tS <- results$tradeStats
idx <- order(tS[,1],tS[,2])
tS <- tS[idx,]
PerformanceAnalytics:::textplot(t(tS)[,1:10])


###################################################
### code chunk number 60: quantstrat-IV.Rnw:792-793
###################################################
dev.off()


###################################################
### code chunk number 61: quantstrat-IV.Rnw:809-810
###################################################
png(file="plot-NETPROFITHEAT.png",height=5,width=5.5,units="in",res=300)


###################################################
### code chunk number 62: quantstrat-IV.Rnw:812-819
###################################################
# net profit
z <- tapply(X=tS[,"End.Equity"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
z[1:5,1:10]
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Net Profit")


###################################################
### code chunk number 63: quantstrat-IV.Rnw:821-822
###################################################
dev.off()


###################################################
### code chunk number 64: quantstrat-IV.Rnw:824-825
###################################################
png(file="plot-MAXDDHEAT.png",height=5,width=5.5,units="in",res=300)


###################################################
### code chunk number 65: quantstrat-IV.Rnw:827-833
###################################################
# maxdd
z <- tapply(X=tS[,"Max.Drawdown"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Max Drawdown")


###################################################
### code chunk number 66: quantstrat-IV.Rnw:835-836
###################################################
dev.off()


###################################################
### code chunk number 67: quantstrat-IV.Rnw:860-861
###################################################
png(file="plot-PFACTORHEAT.png",height=5,width=5.5,units="in",res=300)


###################################################
### code chunk number 68: quantstrat-IV.Rnw:863-869
###################################################
# profit factor
z <- tapply(X=tS[,"Profit.Factor"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Profit Factor")


###################################################
### code chunk number 69: quantstrat-IV.Rnw:871-872
###################################################
dev.off()


###################################################
### code chunk number 70: quantstrat-IV.Rnw:874-875
###################################################
png(file="plot-AVGTRADEHEAT.png",height=5,width=5.5,units="in",res=300)


###################################################
### code chunk number 71: quantstrat-IV.Rnw:877-883
###################################################
# avg trade P&L
z <- tapply(X=tS[,"Avg.Trade.PL"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Average Trade")


###################################################
### code chunk number 72: quantstrat-IV.Rnw:885-886
###################################################
dev.off()


###################################################
### code chunk number 73: quantstrat-IV.Rnw:910-911
###################################################
png(file="plot-RET2MDDHEAT.png",height=5,width=5.5,units="in",res=300)


###################################################
### code chunk number 74: quantstrat-IV.Rnw:913-920
###################################################
# return to maxdd
z <- tapply(X=tS[,"Profit.To.Max.Draw"],
  INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Return to Max Drawdown")


###################################################
### code chunk number 75: quantstrat-IV.Rnw:922-923
###################################################
dev.off()


###################################################
### code chunk number 76: RET2MDDBAR
###################################################
rmdd <- tS$Profit.To.Max.Draw
idx <- order(rmdd,decreasing=T)[1:30]
labs <- paste(tS$nFAST[idx],tS$nSLOW[idx],sep="/")
barplot(rmdd[idx],names.arg=labs,col=4,las=2,main="Return to MaxDrawdown")


###################################################
### code chunk number 77: quantstrat-IV.Rnw:953-954
###################################################
args(tradeGraphs)


###################################################
### code chunk number 78: quantstrat-IV.Rnw:956-960 (eval = FALSE)
###################################################
## 
## tradeGraphs (stats = tS, free.params = c("nFAST", "nSLOW"),
##   statistics = c("Profit.To.Max.Draw","Net.Trading.PL", "Max.Drawdown",
##   "Avg.Trade.PL", "Num.Trades", "Profit.Factor"), title = '')


###################################################
### code chunk number 79: quantstrat-IV.Rnw:1010-1012
###################################################
rm.strat(strategy.st)
strategy(strategy.st, store=TRUE)


###################################################
### code chunk number 80: quantstrat-IV.Rnw:1014-1021
###################################################
add.indicator(strategy.st, name = "SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .fast
	),
	label="nFast"
)


###################################################
### code chunk number 81: quantstrat-IV.Rnw:1023-1030
###################################################
add.indicator(strategy.st, name="SMA",
	arguments = list(
		x = quote(Cl(mktdata)[,1]),
		n = .slow
	),
	label="nSlow"
)


###################################################
### code chunk number 82: quantstrat-IV.Rnw:1038-1045
###################################################
add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="gte"
	),
	label='long'
)


###################################################
### code chunk number 83: quantstrat-IV.Rnw:1047-1054
###################################################
add.signal(strategy.st, name='sigCrossover',
	arguments = list(
		columns=c("nFast","nSlow"),
		relationship="lt"
	),
	label='short'
)


###################################################
### code chunk number 84: quantstrat-IV.Rnw:1062-1078
###################################################
add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=FALSE,
		orderside='long' ,
		ordertype='stoplimit',
		prefer='High',
		threshold=.threshold,
		TxnFees=0,
		orderqty=+.orderqty,
		osFUN=osMaxPos,
		orderset='ocolong'
	),
	type='enter',
	timespan = .timespan,
	label='EnterLONG'
)


###################################################
### code chunk number 85: quantstrat-IV.Rnw:1091-1107
###################################################
add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=FALSE,
		orderside='short',
		ordertype='stoplimit',
		prefer='Low',
		threshold=.threshold,
		TxnFees=0,
		orderqty=-.orderqty,
		osFUN=osMaxPos,
		orderset='ocoshort'
	),
	type='enter',
	timespan = .timespan,
	label='EnterSHORT'
)


###################################################
### code chunk number 86: quantstrat-IV.Rnw:1120-1133
###################################################
add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='short', sigval=TRUE,
		replace=TRUE,
		orderside='long' ,
		ordertype='market',
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocolong'
	),
	type='exit',
	timespan = .timespan,
	label='Exit2SHORT'
)


###################################################
### code chunk number 87: quantstrat-IV.Rnw:1145-1158
###################################################
add.rule(strategy.st, name = 'ruleSignal',
	arguments=list(sigcol='long' , sigval=TRUE,
		replace=TRUE,
		orderside='short',
		ordertype='market',
		TxnFees=.txnfees,
		orderqty='all',
		orderset='ocoshort'
	),
	type='exit',
	timespan = .timespan,
	label='Exit2LONG'
)


###################################################
### code chunk number 88: quantstrat-IV.Rnw:1170-1185
###################################################
add.rule(strategy.st, name = 'ruleSignal',
    arguments=list(sigcol='long' , sigval=TRUE,
        replace=FALSE,
        orderside='long',
        ordertype='stoplimit',
        tmult=TRUE,
        threshold=quote(.stoploss),
        TxnFees=.txnfees,
        orderqty='all',
        orderset='ocolong'
    ),
    type='chain', parent='EnterLONG',
    label='StopLossLONG',
    enabled=FALSE
)


###################################################
### code chunk number 89: quantstrat-IV.Rnw:1200-1215
###################################################
add.rule(strategy.st, name = 'ruleSignal',
    arguments=list(sigcol='short' , sigval=TRUE,
        replace=FALSE,
        orderside='short',
        ordertype='stoplimit',
        tmult=TRUE,
        threshold=quote(.stoploss),
        TxnFees=.txnfees,
        orderqty='all',
        orderset='ocoshort'
    ),
    type='chain', parent='EnterSHORT',
    label='StopLossSHORT',
    enabled=FALSE
)


###################################################
### code chunk number 90: quantstrat-IV.Rnw:1230-1232
###################################################
rm.strat(portfolio.st)
rm.strat(account.st)


###################################################
### code chunk number 91: quantstrat-IV.Rnw:1234-1243
###################################################
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.orderqty)

initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)


###################################################
### code chunk number 92: quantstrat-IV.Rnw:1245-1246
###################################################
enable.rule('luxor', 'chain', 'StopLoss')


###################################################
### code chunk number 93: quantstrat-IV.Rnw:1257-1260
###################################################
out <- applyStrategy(strategy.st, portfolio.st)
updatePortf(portfolio.st, Symbols='GBPUSD',
  Dates=paste('::',as.Date(Sys.time()),sep=''))


###################################################
### code chunk number 94: CHARTLUXOR1SL
###################################################
chart.Posn(portfolio.st,"GBPUSD",TA="add_SMA(n=10,col=2);add_SMA(n=30,col=4)",
  theme=myTheme)


###################################################
### code chunk number 95: quantstrat-IV.Rnw:1266-1267
###################################################
pdf(file="plot-LUXORSTATS1SL.pdf",height=8.5,width=5.5)


###################################################
### code chunk number 96: quantstrat-IV.Rnw:1269-1270
###################################################
PerformanceAnalytics:::textplot(t(tradeStats(portfolio.st, 'GBPUSD')))


###################################################
### code chunk number 97: quantstrat-IV.Rnw:1272-1273
###################################################
dev.off()


###################################################
### code chunk number 98: quantstrat-IV.Rnw:1275-1276
###################################################
pdf(file="plot-ORDERBOOKSL.pdf",height=6,width=9)


###################################################
### code chunk number 99: quantstrat-IV.Rnw:1278-1281
###################################################
ob <- getOrderBook(portfolio.st)$forex$GBPUSD
ob.df <- data.frame(Date=time(ob),coredata(ob))
PerformanceAnalytics:::textplot(ob.df,show.rownames=F)


###################################################
### code chunk number 100: quantstrat-IV.Rnw:1283-1284
###################################################
dev.off()


###################################################
### code chunk number 101: quantstrat-IV.Rnw:1286-1287
###################################################
pdf(file="plot-PERTRADESTATSSL.pdf",height=6,width=9)


###################################################
### code chunk number 102: quantstrat-IV.Rnw:1289-1291
###################################################
PerformanceAnalytics:::textplot(perTradeStats(portfolio.st,"GBPUSD"),
  show.rownames=F)


###################################################
### code chunk number 103: quantstrat-IV.Rnw:1293-1294
###################################################
dev.off()


###################################################
### code chunk number 104: quantstrat-IV.Rnw:1296-1297
###################################################
pdf(file="plot-MAESL.pdf",height=5,width=5)


###################################################
### code chunk number 105: quantstrat-IV.Rnw:1299-1300
###################################################
chart.ME(portfolio.st,'GBPUSD',type='MAE',scale='percent')


###################################################
### code chunk number 106: quantstrat-IV.Rnw:1302-1303
###################################################
dev.off()


###################################################
### code chunk number 107: quantstrat-IV.Rnw:1353-1360
###################################################
add.distribution(strategy.st,
	paramset.label = 'StopLoss',
	component.type = 'chain',
	component.label = 'StopLossLONG',
	variable = list(threshold = .StopLoss),
	label = 'StopLossLONG'
)


###################################################
### code chunk number 108: quantstrat-IV.Rnw:1372-1379
###################################################
add.distribution(strategy.st,
	paramset.label = 'StopLoss',
	component.type = 'chain',
	component.label = 'StopLossSHORT',
	variable = list(threshold = .StopLoss),
	label = 'StopLossSHORT'
)


###################################################
### code chunk number 109: quantstrat-IV.Rnw:1391-1398
###################################################
add.constraint(strategy.st,
	paramset.label = 'StopLoss',
	distribution.label.1 = 'StopLossLONG',
	distribution.label.2 = 'StopLossSHORT',
	operator = '==',
	label = 'StopLoss'
)


###################################################
### code chunk number 110: quantstrat-IV.Rnw:1409-1411
###################################################
rm.strat(portfolio.st)
rm.strat(account.st)


###################################################
### code chunk number 111: quantstrat-IV.Rnw:1413-1414
###################################################
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')


###################################################
### code chunk number 112: quantstrat-IV.Rnw:1416-1421
###################################################
addPosLimit(
            portfolio=portfolio.st,
            symbol='GBPUSD',
            timestamp=initDate,
            maxpos=.orderqty)


###################################################
### code chunk number 113: quantstrat-IV.Rnw:1423-1425
###################################################
initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)


###################################################
### code chunk number 114: quantstrat-IV.Rnw:1427-1428
###################################################
enable.rule('luxor', 'chain', 'StopLoss')


###################################################
### code chunk number 115: quantstrat-IV.Rnw:1439-1458
###################################################
if( Sys.info()['sysname'] == "Windows" )
{
  if(file.exists("resultsSLOpt.RData"))
  {
    load("resultsSLOpt.RData")
  } else {
    results <- apply.paramset(strategy.st, paramset.label='StopLoss',
      portfolio.st=portfolio.st, account.st=account.st, nsamples=5, verbose=TRUE)
  }
} else {
  if(file.exists("resultsSLOpt.RData"))
  {
    load("resultsSLOpt.RData")
  } else {
    results <- apply.paramset(strategy.st, paramset.label='StopLoss',
      portfolio.st=portfolio.st, account.st=account.st, verbose=TRUE)
    save(list="results",file="resultsSLOpt.RData")
  }
}


###################################################
### code chunk number 116: quantstrat-IV.Rnw:1460-1462 (eval = FALSE)
###################################################
## results <- apply.paramset(strategy.st, paramset.label='StopLoss',
##   portfolio.st=portfolio.st, account.st=account.st, nsamples=80, verbose=TRUE)


###################################################
### code chunk number 117: quantstrat-IV.Rnw:1464-1465
###################################################
pdf(file="plot-PARAMSETTRADESTATSSL.pdf",height=6,width=9)


###################################################
### code chunk number 118: quantstrat-IV.Rnw:1467-1471
###################################################
tS <- results$tradeStats
idx <- order(tS[,1])
tS <- tS[idx,]
PerformanceAnalytics:::textplot(t(tS)[,1:5])


###################################################
### code chunk number 119: quantstrat-IV.Rnw:1473-1474
###################################################
dev.off()


###################################################
### code chunk number 120: STOPLOSSANAL
###################################################
par(mfrow=c(1,3))
plot(100*tS$StopLossLONG, tS$Net.Trading.PL, type='b', xlab='Stoploss %',
  ylab='Net.Trading.PL', main='Net Profit vs Stop Loss',col=4)
plot(100*tS$StopLossLONG, tS$Max.Drawdown, type='b', xlab='Stoploss %',
  ylab='Max.Drawdown', main='MaxDrawdown vs Stop Loss',col=4)
plot(100*tS$StopLossLONG, tS$Profit.To.Max.Draw, type='b', xlab='Stoploss %',
  ylab='Profit.To.Max.Draw', main='Return/MaxDD vs Stop Loss',col=4)
par(mfrow=c(1,1))


