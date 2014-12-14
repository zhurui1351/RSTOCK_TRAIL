require(quantmod)
require(testthat)

#构造数据,虚拟数据结构s
data = data.frame(index=c('2000-09-26','2000-09-27','2000-10-26','2000-10-27','2000-11-26'),
                  Open=c(1.49,1.51,1.48,1.45,1.46),
                  High =c(1.52,1.59,1.46,1.44,1.47),
                  Low=c(0.89,0.98,0.98,0.98,0.98),
                  Close=c(1.52,1.59,1.46,1.44,1.47),
                  volatility=c(1,1,-1,-1,1),
                  weekday=c("1","2","1","5","1"),
                  weekth=c("39","39","40","40","41"))

data =xts(data[,2:8],order.by = as.POSIXct(data[,1]))

#满足周一涨的数据,则周二开盘买，收牌平仓。用规则c(1,0,0,0,0)表示周一涨，交易周期按5天计算

result = getTradeInfoInOneWeekByRule(c(1,0,0,0,0),data,2,2,tradeDays = 5)
#期望返回结果，只有39周的周二满足，当天买卖
expect_value1 = data[2,]

expect_equal(result[[1]]$data,data[2,])
expect_equal(result[[1]]$type,'buy')

expect_value1 = data[2,]

expect_equal(result[[2]]$data,data[2,])
expect_equal(result[[2]]$type,'sell')
