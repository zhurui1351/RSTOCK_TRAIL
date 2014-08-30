### R code from vignette source 'quantstrat-III.Rnw'

###################################################
### code chunk number 1: "House keeping"
###################################################
options(width=82,continue=" ")
suppressWarnings(try(rm(list=ls()),silent=TRUE))


###################################################
### code chunk number 2: quantstrat-III.Rnw:123-128
###################################################
library(quantstrat)
startDate <- '2010-01-01'  # start of data
endDate <-  '2013-07-31'   # end of data
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
Sys.setenv(TZ="UTC")       # set time zone


###################################################
### code chunk number 3: quantstrat-III.Rnw:130-138
###################################################
#if(file.exists("XLX.RData"))
#{
#  load("XLX.RData")
#} else {
  getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
    from=startDate, to=endDate)
#  save(list=symbols,file="XLX.RData")
#}


###################################################
### code chunk number 4: quantstrat-III.Rnw:140-142 (eval = FALSE)
###################################################
## getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
##   from=startDate, to=endDate)


###################################################
### code chunk number 5: quantstrat-III.Rnw:154-158
###################################################
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'


###################################################
### code chunk number 6: quantstrat-III.Rnw:160-161
###################################################
pdf(file="plot-XLX3x3.pdf",width=18,height=12,pointsize=8)


###################################################
### code chunk number 7: quantstrat-III.Rnw:163-169
###################################################
par(mfrow=c(3,3))
for(symbol in symbols)
{
  plot(chart_Series(get(symbol),name=symbol,theme=myTheme))
}
par(mfrow=c(1,1))


###################################################
### code chunk number 8: quantstrat-III.Rnw:171-172
###################################################
dev.off()


###################################################
### code chunk number 9: quantstrat-III.Rnw:190-194
###################################################
initDate <- '2009-12-31'
initEq <- 1e6
currency("USD")
stock(symbols, currency="USD",multiplier=1)


###################################################
### code chunk number 10: XLFBB
###################################################
args(BBands)
b <- BBands(HLC=HLC(XLF["2013"]), n=20, sd=2)
tail(b)
chart_Series(XLF["2013"],TA='add_BBands(lwd=2)',theme=myTheme,name="XLF")


###################################################
### code chunk number 11: quantstrat-III.Rnw:267-272
###################################################
rm.strat("multiAsset.bb1") # remove portfolio, account, orderbook if re-run
initPortf(name="multiAsset.bb1", symbols, initDate=initDate)
initAcct(name="multiAsset.bb1", portfolios="multiAsset.bb1",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multiAsset.bb1", initDate=initDate)


###################################################
### code chunk number 12: quantstrat-III.Rnw:283-284
###################################################
strategy("bbands", store=TRUE)


###################################################
### code chunk number 13: quantstrat-III.Rnw:286-287
###################################################
args(BBands)


###################################################
### code chunk number 14: quantstrat-III.Rnw:289-291
###################################################
add.indicator("bbands", name = "BBands",
  arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='BBands')


###################################################
### code chunk number 15: quantstrat-III.Rnw:305-308
###################################################
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("Close","up"),relationship="gt"),
  label="Cl.gt.UpperBand")


###################################################
### code chunk number 16: quantstrat-III.Rnw:310-313
###################################################
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("Close","dn"),relationship="lt"),
  label="Cl.lt.LowerBand")


###################################################
### code chunk number 17: quantstrat-III.Rnw:315-318
###################################################
add.signal("bbands", name="sigCrossover",
  arguments=list(columns=c("High","Low","mavg"),relationship="op"),
  label="Cross.Mid")


###################################################
### code chunk number 18: quantstrat-III.Rnw:329-332
###################################################
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-100,
  ordertype='market', orderside=NULL, threshold=NULL),type='enter')


###################################################
### code chunk number 19: quantstrat-III.Rnw:334-337
###################################################
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 100,
  ordertype='market', orderside=NULL, threshold=NULL),type='enter')


###################################################
### code chunk number 20: quantstrat-III.Rnw:339-342
###################################################
add.rule("bbands", name='ruleSignal',
  arguments=list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all',
  ordertype='market', orderside=NULL, threshold=NULL),type='exit')


###################################################
### code chunk number 21: quantstrat-III.Rnw:356-358
###################################################
SD = 2
N = 20


###################################################
### code chunk number 22: quantstrat-III.Rnw:360-362
###################################################
out <- applyStrategy("bbands",
  portfolios="multiAsset.bb1",parameters=list(sd=SD,n=N))


###################################################
### code chunk number 23: quantstrat-III.Rnw:376-379
###################################################
updatePortf("multiAsset.bb1")
updateAcct("multiAsset.bb1")
updateEndEq("multiAsset.bb1")


###################################################
### code chunk number 24: quantstrat-III.Rnw:389-417
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
checkBlotterUpdate("multiAsset.bb1","multiAsset.bb1")


###################################################
### code chunk number 25: XLBCP
###################################################
chart.Posn("multiAsset.bb1","XLB",TA="add_BBands(n=20,sd=2)",theme=myTheme)


###################################################
### code chunk number 26: XLBCP2
###################################################
chart.Posn("multiAsset.bb1","XLB",TA="add_BBands(n=20,sd=2)",
  Dates="2010",theme=myTheme)


###################################################
### code chunk number 27: quantstrat-III.Rnw:457-462
###################################################
rm.strat("multiAsset.bb2") # remove portfolio, account, orderbook if re-run
initPortf(name="multiAsset.bb2", symbols, initDate=initDate)
initAcct(name="multiAsset.bb2", portfolios="multiAsset.bb2",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multiAsset.bb2", initDate=initDate)


###################################################
### code chunk number 28: quantstrat-III.Rnw:464-467
###################################################
SD=3
out <- applyStrategy("bbands",
  portfolios="multiAsset.bb2",parameters=list(sd=SD,n=N))


###################################################
### code chunk number 29: quantstrat-III.Rnw:469-472
###################################################
updatePortf("multiAsset.bb2")
updateAcct("multiAsset.bb2")
updateEndEq("multiAsset.bb2")


###################################################
### code chunk number 30: quantstrat-III.Rnw:483-484
###################################################
checkBlotterUpdate("multiAsset.bb2","multiAsset.bb2")


###################################################
### code chunk number 31: BBCUMRET
###################################################
eq1 <- getAccount("multiAsset.bb1")$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")
eq2 <- getAccount("multiAsset.bb2")$summary$End.Eq
rt2 <- Return.calculate(eq2,"log")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("SD=2","SD=3")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
main="BBand SD Parameter Comparison",ylab="cum return",xlab="")


###################################################
### code chunk number 32: quantstrat-III.Rnw:531-532
###################################################
args(ruleSignal)


###################################################
### code chunk number 33: quantstrat-III.Rnw:550-551
###################################################
args(osNoOp)


###################################################
### code chunk number 34: quantstrat-III.Rnw:568-574
###################################################
osFixedDollar <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/ClosePrice,-2)
  return(orderqty)
}


###################################################
### code chunk number 35: XLVMACD
###################################################
args(MACD)
macd  <- MACD( Cl(XLV), 12, 26, 9, maType="EMA" )
tail(macd,3)
chart_Series(XLV,
  TA="add_MACD();add_EMA(12,col='darkgreen');add_EMA(26,col='blue')",
  subset="20100717::20101208",theme=myTheme)


###################################################
### code chunk number 36: quantstrat-III.Rnw:674-679
###################################################
rm.strat("multi.macd") # remove portfolio, account, orderbook if re-run
initPortf(name="multi.macd", symbols, initDate=initDate)
initAcct(name="multi.macd", portfolios="multi.macd",initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd", initDate=initDate)


###################################################
### code chunk number 37: quantstrat-III.Rnw:690-691
###################################################
strategy("macd", store=TRUE)


###################################################
### code chunk number 38: quantstrat-III.Rnw:693-695
###################################################
add.indicator("macd", name = "MACD",
  arguments = list(x=quote(Cl(mktdata))),label='osc')


###################################################
### code chunk number 39: quantstrat-III.Rnw:697-700
###################################################
add.signal("macd",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="gt",threshold=0,cross=TRUE),
  label="signal.gt.zero")


###################################################
### code chunk number 40: quantstrat-III.Rnw:702-705
###################################################
add.signal("macd",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="lt",threshold=0,cross=TRUE),
  label="signal.lt.zero")


###################################################
### code chunk number 41: quantstrat-III.Rnw:716-720
###################################################
add.rule("macd",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero",sigval=TRUE,orderqty=100,
  ordertype='market',orderside='long',threshold=NULL,osFUN='osFixedDollar'),
  type='enter',label='enter',storefun=FALSE)


###################################################
### code chunk number 42: quantstrat-III.Rnw:722-726
###################################################
add.rule("macd",name='ruleSignal',
  arguments = list(sigcol="signal.lt.zero",sigval=TRUE,orderqty='all',
  ordertype='market',orderside='long',threshold=NULL,orderset='exit2'),
  type='exit',label='exit')


###################################################
### code chunk number 43: quantstrat-III.Rnw:741-746
###################################################
fastMA = 12
slowMA = 26
signalMA = 9
maType="EMA"
tradeSize <- initEq/10


###################################################
### code chunk number 44: quantstrat-III.Rnw:748-751
###################################################
out<-applyStrategy("macd" , portfolios="multi.macd",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)


###################################################
### code chunk number 45: quantstrat-III.Rnw:762-765
###################################################
updatePortf("multi.macd")
updateAcct("multi.macd")
updateEndEq("multi.macd")


###################################################
### code chunk number 46: quantstrat-III.Rnw:773-774
###################################################
checkBlotterUpdate("multi.macd","multi.macd")


###################################################
### code chunk number 47: MACDCPOSN1
###################################################
chart.Posn(Portfolio="multi.macd",Symbol="XLV",theme=myTheme)


###################################################
### code chunk number 48: quantstrat-III.Rnw:788-789
###################################################
png(file="plot-MACDCPOSN2.png",width=9,height=6,units="in",res=300)


###################################################
### code chunk number 49: quantstrat-III.Rnw:791-796
###################################################
chart.Posn(Portfolio="multi.macd",Symbol="XLV",
  Dates="201006::20101213",theme=myTheme)
add_MACD()
add_EMA(12,col='red')
add_EMA(26,col='blue')


###################################################
### code chunk number 50: quantstrat-III.Rnw:798-799
###################################################
dev.off()


###################################################
### code chunk number 51: quantstrat-III.Rnw:823-824
###################################################
options(width=105)


###################################################
### code chunk number 52: quantstrat-III.Rnw:826-827
###################################################
perTradeStats("multi.macd","XLV")


###################################################
### code chunk number 53: quantstrat-III.Rnw:829-830
###################################################
options(width=82)


###################################################
### code chunk number 54: quantstrat-III.Rnw:883-884
###################################################
strategy("bb.lim", store=TRUE)


###################################################
### code chunk number 55: quantstrat-III.Rnw:886-888
###################################################
add.indicator("bb.lim", name = "BBands",
  arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='BBands')


###################################################
### code chunk number 56: quantstrat-III.Rnw:890-893
###################################################
add.signal("bb.lim", name="sigCrossover",
  arguments=list(columns=c("Close","up"),relationship="gt"),
  label="Cl.gt.UpperBand")


###################################################
### code chunk number 57: quantstrat-III.Rnw:895-898
###################################################
add.signal("bb.lim", name="sigCrossover",
  arguments=list(columns=c("Close","dn"),relationship="lt"),
  label="Cl.lt.LowerBand")


###################################################
### code chunk number 58: quantstrat-III.Rnw:900-903
###################################################
add.signal("bb.lim", name="sigCrossover",
  arguments=list(columns=c("High","Low","mavg"),relationship="op"),
  label="Cross.Mid")


###################################################
### code chunk number 59: quantstrat-III.Rnw:911-915
###################################################
add.rule("bb.lim", name='ruleSignal',
  arguments=list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-1000,
  ordertype='market', orderside=NULL, threshold=NULL, osFUN='osMaxPos'),
  type='enter')


###################################################
### code chunk number 60: quantstrat-III.Rnw:917-921
###################################################
add.rule("bb.lim", name='ruleSignal',
  arguments=list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 1000,
  ordertype='market', orderside=NULL, threshold=NULL, osFUN='osMaxPos'),
  type='enter')


###################################################
### code chunk number 61: quantstrat-III.Rnw:923-926
###################################################
add.rule("bb.lim", name='ruleSignal',
  arguments=list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all',
  ordertype='market', orderside=NULL, threshold=NULL),type='exit')


###################################################
### code chunk number 62: quantstrat-III.Rnw:939-940
###################################################
args(addPosLimit)


###################################################
### code chunk number 63: quantstrat-III.Rnw:959-964
###################################################
rm.strat("multi.bb.limit") # remove portfolio, account, orderbook if re-run
initPortf(name="multi.bb.limit", symbols, initDate=initDate)
initAcct(name="multi.bb.limit", portfolios="multi.bb.limit",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.bb.limit", initDate=initDate)


###################################################
### code chunk number 64: quantstrat-III.Rnw:966-970
###################################################
for(symbol in symbols)
{
  addPosLimit("multi.bb.limit", symbol, initDate, 200, 2 )
}


###################################################
### code chunk number 65: quantstrat-III.Rnw:981-983
###################################################
SD = 2
N = 20


###################################################
### code chunk number 66: quantstrat-III.Rnw:985-987
###################################################
out <- applyStrategy("bb.lim",
  portfolios="multi.bb.limit",parameters=list(sd=SD,n=N))


###################################################
### code chunk number 67: quantstrat-III.Rnw:989-992
###################################################
updatePortf("multi.bb.limit")
updateAcct("multi.bb.limit")
updateEndEq("multi.bb.limit")


###################################################
### code chunk number 68: XLBCPLIM
###################################################
chart.Posn("multi.bb.limit","XLB",TA="add_BBands(n=20,sd=2)",theme=myTheme)


###################################################
### code chunk number 69: XLBCPLIM2
###################################################
chart.Posn("multi.bb.limit","XLB",TA="add_BBands(n=20,sd=2)",
  Dates="2010",theme=myTheme)


###################################################
### code chunk number 70: quantstrat-III.Rnw:1007-1008
###################################################
checkBlotterUpdate("multi.bb.limit","multi.bb.limit")


###################################################
### code chunk number 71: quantstrat-III.Rnw:1065-1066
###################################################
strategy("faber",store=TRUE)


###################################################
### code chunk number 72: quantstrat-III.Rnw:1068-1070
###################################################
add.indicator(strategy = "faber", name = "SMA",
  arguments = list(x = quote(Cl(mktdata))), label="SMAn")


###################################################
### code chunk number 73: quantstrat-III.Rnw:1072-1075
###################################################
add.signal("faber",name="sigCrossover",
  arguments = list(columns=c("Close","SMAn"),relationship="gt"),
  label="Cl.gt.SMA")


###################################################
### code chunk number 74: quantstrat-III.Rnw:1077-1080
###################################################
add.signal("faber",name="sigCrossover",
  arguments = list(columns=c("Close","SMAn"),relationship="lt"),
  label="Cl.lt.SMA")


###################################################
### code chunk number 75: quantstrat-III.Rnw:1091-1095
###################################################
add.rule("faber", name='ruleSignal',
  arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=100000,
  ordertype='market', orderside='long', osFUN='osMaxPos'),
  type='enter', path.dep=TRUE)


###################################################
### code chunk number 76: quantstrat-III.Rnw:1097-1101
###################################################
add.rule("faber", name='ruleSignal',
  arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
  ordertype='market', orderside='long', pricemethod='market'),
  type='exit', path.dep=TRUE)


###################################################
### code chunk number 77: quantstrat-III.Rnw:1115-1116
###################################################
args(rulePctEquity)


###################################################
### code chunk number 78: quantstrat-III.Rnw:1130-1140
###################################################
add.rule('faber', 'rulePctEquity',
  arguments=list(
    rebalance_on='months',
    trade.percent=1/length(symbols),
    refprice=quote(last(getPrice(mktdata)[paste('::',timestamp,sep='')])),
    digits=0
  ),
  type='rebalance',
  label='rebalance'
)


###################################################
### code chunk number 79: quantstrat-III.Rnw:1151-1152
###################################################
rm.strat("multi.faber") # remove portfolio, account, orderbook if re-run


###################################################
### code chunk number 80: quantstrat-III.Rnw:1154-1158
###################################################
initPortf(name="multi.faber", symbols, initDate=initDate)
initAcct(name="multi.faber", portfolios="multi.faber",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.faber", initDate=initDate)


###################################################
### code chunk number 81: quantstrat-III.Rnw:1160-1165
###################################################
(posval <- initEq/length(symbols))
for(symbol in symbols){
    pos<-round((posval/first(getPrice(get(symbol)))),-2)
    addPosLimit('multi.faber',symbol,initDate, maxpos=pos,minpos=-pos)
}


###################################################
### code chunk number 82: quantstrat-III.Rnw:1174-1176
###################################################
out <- applyStrategy.rebalancing(strategy="faber", portfolios="multi.faber",
  parameters=list(n=200))


###################################################
### code chunk number 83: quantstrat-III.Rnw:1178-1181
###################################################
updatePortf("multi.faber")
updateAcct("multi.faber")
updateEndEq("multi.faber")


###################################################
### code chunk number 84: XLKCPF
###################################################
chart.Posn("multi.faber","XLF",TA="add_SMA(n=200)",theme=myTheme)


###################################################
### code chunk number 85: quantstrat-III.Rnw:1192-1193
###################################################
checkBlotterUpdate("multi.faber","multi.faber")


###################################################
### code chunk number 86: quantstrat-III.Rnw:1210-1211
###################################################
options(width=100)


###################################################
### code chunk number 87: XLUBAR
###################################################
(pts <- perTradeStats("multi.faber","XLU"))
mnc <- pts$Max.Notional.Cost
pe <- sapply(pts$Start,getEndEq,Account="multi.faber")/9
barplot(rbind(pe,mnc),beside=T,col=c(2,4),names.arg=format(pts$Start,"%m/%d/%y"),
  ylim=c(0,1.5e5),ylab="$",xlab="Trade Date")
legend(x="topleft",legend=c("(Portfolio Equity)/9","Order Size"),pch=15,col=c(2,4),bty="n")
title("Percent of Portfolio Equity versus Trade Size for XLU")


###################################################
### code chunk number 88: quantstrat-III.Rnw:1222-1223
###################################################
options(width=82)


