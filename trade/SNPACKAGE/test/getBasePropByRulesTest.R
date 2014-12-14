require(quantmod)
require(testthat)
#构造数据
data = data.frame(index=c('2000-09-26','2000-09-27','2000-10-26','2000-10-27','2000-11-26'),
                  Open=c(1.49,1.51,1.48,1.45,1.46),
                  High =c(1.52,1.59,1.46,1.44,1.47),
                  Low=c(0.89,0.98,0.98,0.98,0.98),
                  Close=c(1.52,1.59,1.46,1.44,1.47),
                  volatility=c(1,1,-1,-1,1),
                  weekday=c("1","2","4","5","5"),
                  weekth=c("39","39","40","40","41"))

data = xts(data[,2:8],order.by = as.POSIXct(data[,1]))

rules = list(c(0,0,0,0,1),c(1,0,0,0,0))

base_prob = getBasePropByRules(rules,data)

#期望返回值
expected_value1 = list(rule=c(0,0,0,0,1),ratio=0.3333333,num_satisfied = 1,numweek=3)
expected_value2 = list(rule=c(1,0,0,0,0),ratio=0.3333333,num_satisfied = 1,numweek=3)


expect_equal(expected_value1,base_prob[[1]],tolerance = .002)

expect_equal(expected_value2,base_prob[[2]],tolerance = .002)
