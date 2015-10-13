#https://quantstrattrader.wordpress.com/category/hypothesis-driven-development/
#http://seekingalpha.com/article/3461016-a-conservative-tactical-momentum-strategy-using-bond-etfs

require(quantmod)
require(PerformanceAnalytics)
require(reshape2)
symbols <- c("CNSAX", "FAHDX", "VUSTX", "VFISX", "PREMX")
getSymbols(symbols, from='1900-01-01')
prices <- list()
for(symbol in symbols) {
  prices[[symbol]] <- Ad(get(symbol))
}
prices <- do.call(cbind, prices)
colnames(prices) <- substr(colnames(prices), 1, 5)
#returns <- na.omit(Return.calculate(prices))
x = Return.calculate(prices[,1])
for(i in 2:5)
{
  y = Return.calculate(prices[,i])
  x = merge(x,y)
}
returns <- x
sample <- returns['1997-08/2009-03']
monthRets <- apply.monthly(sample, Return.cumulative)

returnRegression <- function(returns, nMonths) {
  nMonthAverage <- apply(returns, 2, runSum, n = nMonths)
  nMonthAverage <- xts(nMonthAverage, order.by = index(returns))
  #nMonthAverage <- na.omit(lag(nMonthAverage))
  x = lag(prices[,1])
  for(i in 2:5)
  {
    y = lag(prices[,i])
    x = merge(x,y)
  }
  nMonthAverage = na.omit(x)
  returns <- returns[index(nMonthAverage)]
  
  rankAvg <- t(apply(nMonthAverage, 1, rank))
  rankReturn <- t(apply(returns, 1, rank))
  
  
  meltedAverage <- melt(data.frame(nMonthAverage))
  meltedReturns <- melt(data.frame(returns))
  meltedRankAvg <- melt(data.frame(rankAvg))
  meltedRankReturn <- melt(data.frame(rankReturn))
  lmfit <- lm(meltedReturns$value ~ meltedAverage$value - 1)
  rankLmfit <- lm(meltedRankReturn$value ~ meltedRankAvg$value)
  return(rbind(summary(lmfit)$coefficients, summary(rankLmfit)$coefficients))
}

pvals <- list()
estimates <- list()
rankPs <- list()
rankEstimates <- list()
for(i in 1:24) {
  tmp <- returnRegression(monthRets, nMonths=i)
  pvals[[i]] <- tmp[1,4]
  estimates[[i]] <- tmp[1,1]
  rankPs[[i]] <- tmp[2,4]
  rankEstimates[[i]] <- tmp[2,1]
}
pvals <- do.call(c, pvals)
estimates <- do.call(c, estimates)
rankPs <- do.call(c, rankPs)
rankEstimates <- do.call(c, rankEstimates)


plot(estimates, type='h', xlab = 'Months regressed on', ylab='momentum coefficient', 
     main='future returns regressed on past momentum')
plot(pvals, type='h', xlab='Months regressed on', ylab='p-value', main='momentum significance')
abline(h=.05, col='green')
abline(h=.1, col='red')

plot(rankEstimates, type='h', xlab='Months regressed on', ylab="Rank coefficient",
     main='future return ranks regressed on past momentum ranks', ylim=c(0,3))
plot(rankPs, type='h', xlab='Months regressed on', ylab='P-values')

