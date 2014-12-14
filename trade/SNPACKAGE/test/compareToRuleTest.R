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

#周一涨的数据,用规则c(1,0,0,0,0)表示周一涨，交易周期按5天计算

result = compareToRule(c(1,0,0,0,0),data,tradeDays = 5)

#期望得到的结果,满足条件的频率在0.66 有两周满足条件，一共有三周
expected_result = c(0.6666667,2,3)

#由于是浮点数 允许存在一定的误差
expect_equal(expected_result,result,tolerance = .002)
