#rm(list=ls(all=T))
library(quantstrat)
library(quantmod)
library(blotter)
require(blotter)
# 金融产品初始化
currency("RMB")
stock("DBZY", currency = "RMB", multiplier = 1)
# 设定时区
Sys.setenv(TZ = "UTC")
DBZY <- getSymbols("000597.sz",src = "yahoo",auto.assign = FALSE)
DBZY <- to.monthly(DBZY, indexAt = "endof")
DBZY$SMA10m <- SMA(Cl(DBZY), 10)
head(DBZY$SMA10m)
# 初始化组合和账户
q.strategy <- "qFaber"
initPortf(q.strategy, "DBZY", initDate = "2007-01-31")
initAcct(q.strategy, portfolios = q.strategy, initDate = "2007-01-31", initEq = 1e+06)
# 初始化指定和策略
initOrders(portfolio = q.strategy, initDate = "2007-01-31")
strategy(q.strategy, store = TRUE)
ls(all = T) #quantstrat创建了.strategy环境
strategy <- getStrategy(q.strategy)
summary(strategy)
# 加入一个指标，10月均线
add.indicator(strategy = q.strategy, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 10), label = "SMA10")
# 加入信号，向上交叉10月线，向下交叉10月线
add.signal(q.strategy, name = "sigCrossover", arguments = list(columns = c("Close", "SMA10"), relationship = "gt"), label = "Cl.gt.SMA")
add.signal(q.strategy, name = "sigCrossover", arguments = list(columns = c("Close", "SMA10"), relationship = "lt"), label = "Cl.lt.SMA")
# 加入规则，买入规则和卖出规则
add.rule(q.strategy, name = "ruleSignal", arguments = list(sigcol = "Cl.gt.SMA", 
                                                           sigval = TRUE, orderqty = 900, ordertype = "market", orderside = "long", 
                                                           pricemethod = "market"), type = "enter", path.dep = TRUE) # 买入数量为900股

add.rule(q.strategy, name = "ruleSignal", arguments = list(sigcol = "Cl.lt.SMA", 
                                                           sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long", 
                                                           pricemethod = "market"), type = "exit", path.dep = TRUE)

summary(getStrategy(q.strategy))
#现在的策略strategy中有1个指标,2种信号和相应的两个规则（买入、卖出）。

out <- applyStrategy(strategy = q.strategy, portfolios = q.strategy)
summary(out)
mktdata["2011"]
#mktdata是在执行策略过程中创建的一个特殊的变量，包含了原交易数据的时间序列以及指标和信号。
updatePortf(q.strategy)
updateAcct(q.strategy)
updateEndEq(q.strategy)
#最后按照现在的组合和账户情况给出策略表现
myTheme <- chart_theme()
myTheme$col$dn.col <- "lightgreen"
myTheme$col$up.col <- "lightblue"
myTheme$col$dn.border <- "grey"
myTheme$col$up.border <- "grey"
# 策略表现可视化
chart.Posn(q.strategy, Symbol = "DBZY", Dates = "2008::", theme = myTheme)
# 交易统计
(tstats <- tradeStats(Portfolio = q.strategy, Symbol = "DBZY"))
# 指令簿（order book）
ob <- getOrderBook(q.strategy)
head(ob$qFaber$DBZY)
chart.ME(Portfolio = q.strategy, Symbol = "DBZY", type = "MAE", scale = "percent")
chart.ME(Portfolio = q.strategy, Symbol = "DBZY", type = "MFE", scale = "percent")
