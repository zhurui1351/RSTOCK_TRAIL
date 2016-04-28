#https://quantstrattrader.wordpress.com/2016/04/18/are-r2s-useful-in-finance-hypothesis-driven-development-in-reverse/
require(xts)
require(quantmod)
require(PerformanceAnalytics)
require(TTR)

getSymbols('SPY', from = '1990-01-01', src = 'yahoo')
adjustedPrices <- Ad(SPY)
monthlyAdj <- to.monthly(adjustedPrices, OHLC=TRUE)

spySMA <- SMA(Cl(monthlyAdj), 10)
spyROC <- ROC(Cl(monthlyAdj), 10)
spyRets <- Return.calculate(Cl(monthlyAdj))

smaRatio <- Cl(monthlyAdj)/spySMA - 1
smaSig <- smaRatio > 0
rocSig <- spyROC > 0

smaRets <- lag(smaSig) * spyRets
rocRets <- lag(rocSig) * spyRets

strats <- na.omit(cbind(smaRets, rocRets, spyRets))
colnames(strats) <- c("SMA10", "MOM10", "BuyHold")
charts.PerformanceSummary(strats, main = "strategies")
rbind(table.AnnualizedReturns(strats), maxDrawdown(strats), CalmarRatio(strats))

predictorsAndPredicted <- na.omit(cbind(lag(smaRatio), lag(spyROC), spyRets))
R2s <- list()
for(i in 1:(nrow(predictorsAndPredicted)-59))  { #rolling five-year regression
  subset <- predictorsAndPredicted[i:(i+59),]
  smaLM <- lm(subset[,3]~subset[,1])
  smaR2 <- summary(smaLM)$r.squared
  rocLM <- lm(subset[,3]~subset[,2])
  rocR2 <- summary(rocLM)$r.squared
  R2row <- xts(cbind(smaR2, rocR2), order.by=last(index(subset)))
  R2s[[i]] <- R2row
}
R2s <- do.call(rbind, R2s)
par(mfrow=c(1,1))
colnames(R2s) <- c("SMA", "Momentum")
chart.TimeSeries(R2s, main = "R2s", legend.loc = 'topleft')
