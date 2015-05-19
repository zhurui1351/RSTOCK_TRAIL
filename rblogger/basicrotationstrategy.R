#https://quantstrattrader.wordpress.com/2015/05/18/a-basic-logical-invest-global-market-rotation-strategy/
require(quantmod)
require(PerformanceAnalytics)

symbols <- c("MDY", "TLT", "EEM", "ILF", "EPP", "FEZ")
getSymbols(symbols, from="1990-01-01")
prices <- list()
for(i in 1:length(symbols)) {
  prices[[i]] <- Ad(get(symbols[i]))
}

prices <- do.call(cbind, prices)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))
returns <- Return.calculate(prices)
returns <- na.omit(returns)

logicInvestGMR <- function(returns, lookback = 3) {
  ep <- endpoints(returns, on = "months") 
  weights <- list()
  for(i in 2:(length(ep) - lookback)) {
    retSubset <- returns[ep[i]:ep[i+lookback],]
    cumRets <- Return.cumulative(retSubset)
    rankCum <- rank(cumRets)
    weight <- rep(0, ncol(retSubset))
    weight[which.max(cumRets)] <- 1
    weight <- xts(t(weight), order.by=index(last(retSubset)))
    weights[[i]] <- weight
  }
  weights <- do.call(rbind, weights)
  stratRets <- Return.portfolio(R = returns, weights = weights)
  return(stratRets)
}

gmr <- logicInvestGMR(returns)
charts.PerformanceSummary(gmr)
rbind(table.AnnualizedReturns(gmr), maxDrawdown(gmr), CalmarRatio(gmr))
