library(xts)
require(PerformanceAnalytics)
#生成序列
myTs <- xts(rnorm(400)/100,order.by=seq(Sys.Date()-399,Sys.Date(),"days"))
myOtherTs <- xts(rnorm(200)/100,order.by=seq(Sys.Date()-199,Sys.Date(),"days"))
#链接
# dates intersection
tsInter <- merge.xts(myTs,myOtherTs,join="inner")
# dates union and blanks filled with NAs
tsUnion <- merge.xts(myTs,myOtherTs,join="outer")
#apply.monthly
monthlyRtn <- apply.monthly(tsInter[,2], sum)
yearlyRtn <- apply.yearly(tsInter[,2], sum)
#求时间跨度端点
newTs <- myOtherTs[endpoints(tsInter, on="months"),]
#使用最近时间点的值填充na
n <- 100
k <- 5
N <- k*n
prices <- cumsum(rnorm(N))
theSample <- sample(c(1:length(prices)),10)
prices[theSample] <- NA
prices <- na.locf(prices)
#绘制收益 回撤曲线
prices <- xts(prices,order.by=seq(Sys.Date()-(length(prices)-1),Sys.Date(),"days"))
charts.PerformanceSummary(Return.calculate(prices, method="discrete"))
