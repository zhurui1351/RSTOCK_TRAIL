#https://www.r-bloggers.com/simulating-backtests-of-stock-returns-using-monte-carlo-and-snowfall-in-parallel/
# load packages
library(quantmod)

# download DAX data from Yahoo
dax <- getSymbols("^GDAXI", from = "2000-01-01", auto.assign = F)

# create a data.frame Sdata, that contains the stock data
Sdata <- data.frame(date = index(dax), price = as.numeric(Ad(dax)))

# calculate returns
Sdata$rets <- Sdata$price / c(NA, Sdata$price[1:(nrow(Sdata) - 1)]) - 1

head(Sdata)
#         date   price          rets
# 1 2010-01-04 6048.30            NA
# 2 2010-01-05 6031.86 -0.0027181096
# 3 2010-01-06 6034.33  0.0004095279
# 4 2010-01-07 6019.36 -0.0024808413
# 5 2010-01-08 6037.61  0.0030318839
# 6 2010-01-11 6040.50  0.0004786889

# create a first plot to have a look at the price development
plot(x = Sdata$date, y = Sdata$price, type = "l", main = "Dax Development")

#模拟价格数据
get.fprice <- function(rets, perc.right, init.price){
  # 1. sample the goodness of the returns
  good.forecast <- sample(x = c(T, F),
                          size = length(rets),
                          prob = c(perc.right, 1 - perc.right),
                          replace = T)
  
  # 2. get the forecasted directions, the same as the true rets if good.forecast = T
  dir <- ifelse(rets > 0, 1, -1)
  forecast.dir <- ifelse(good.forecast, dir, -dir)
  # if the percentage sampled should be displayed
  # mean(dir == forecast.dir, na.rm = T) 
  
  # 3. calculate the return of the forecast
  forecast.ret <- forecast.dir * rets
  
  # 4. calculate the prices
  forecast.price <- cumprod(1 + forecast.ret[2:length(forecast.ret)]) * init.price
  forecast.price <- c(init.price, forecast.price)
  return(forecast.price)
}

# set a seed for reproducability
set.seed(42)

# simulate one series of prices
Sdata$fprice <- get.fprice(rets = Sdata$rets, perc.right = 0.51,
                           init.price = Sdata$price[1])

# plot the two developments
plot(x = Sdata$date, y = Sdata$price, type = "l", 
     main = "Dax vs. Forecast", xlab = "Date", ylab = "Price",
     ylim = c(0, max(Sdata$price, Sdata$fprice)))
lines(x = Sdata$date, y = Sdata$fprice, col = "red")
legend("topleft", c("Dax", paste(perc.right, "forecast")), 
       col = 1:2, lty = 1)



get.fprice.n <- function(rets, perc.right, init.price, n){
  # create a function that produces the goodness of the forecast
  get.good.forecast <- function(x){
    good.forecast <- sample(x = c(T, F),
                            size = length(rets),
                            prob = c(perc.right, 1 - perc.right),
                            replace = T)
    return(good.forecast)
  }
  
  # 1. sample the goodness of the returns
  good.forecasts <- sapply(1:n, get.good.forecast)
  
  # 2. get the forecasted directions, the same as the true rets if good.forecast = T
  dir <- ifelse(rets > 0, 1, -1)
  forecast.dirs <- apply(good.forecasts, 2, function(x) {
    ifelse(x, dir, -dir)
  })
  
  # 3. calculate the return of the forecast
  forecast.rets <- forecast.dirs * rets
  
  # 4. calculate the prices
  forecast.prices <- apply(forecast.rets, 2, function(x) {
    cumprod(1 + x[2:length(x)]) * init.price
  })
  
  forecast.prices <- rbind(rep(init.price, ncol(forecast.prices)), forecast.prices)
  
  # collapse the n simulations to just one by taking the average
  forecast.price <- apply(forecast.prices, 1, mean)
  return(forecast.price)
}

# simulate 10.000 cases 
# set a seed for reproducability, 
# should not matter due to the Law Of Large Numbers
set.seed(42)

# simulate 10.000 series of prices
t <- Sys.time()
Sdata$fprice <- get.fprice.n(rets = Sdata$rets, 
                             perc.right = 0.51,
                             init.price = Sdata$price[1],
                             n = 10000)
Sys.time() - t # takes 5.69257 seconds on my machine

# plot the two developments
plot(x = Sdata$date, y = Sdata$price, type = "l", 
     main = "Dax vs. Forecasts", xlab = "Date", ylab = "Price",
     ylim = c(0, max(Sdata$price, Sdata$fprice)))
lines(x = Sdata$date, y = Sdata$fprice, col = "red")
legend("topleft", c("Dax", "aggregated forecasts"), 
       col = 1:2, lty = 1)



k <- 4
# the percentages that will be used later on
perc.right <- seq(from = 0.45, to = 0.55, length.out = k)
# [1] 0.4500000 0.4833333 0.5166667 0.5500000

# simulate k cases n times, equals 40.000 times
t <- Sys.time()
forecasted.prices <- sapply(perc.right, function(x) {
  get.fprice.n(rets = Sdata$rets, 
               perc.right = x,
               init.price = Sdata$price[1],
               n = 10000)
})
Sys.time() - t # takes 21.592 seconds on my machine

# plot the results
plot(x = Sdata$date, y = Sdata$price, type = "l", 
     main = "Dax vs. Forecasts", xlab = "Date", ylab = "Price",
     ylim = c(0, max(forecasted.prices, Sdata$price)))
for (i in 1:k){
  lines(x = Sdata$date, y = forecasted.prices[, i], col = (i + 1))
}
legend("topleft", c("Dax", 
                    paste0("P = ", round(perc.right, 2), sep = "")), 
       col = 1:(k + 1), lty = 1, cex = 0.75)