require(quantmod)
require(testthat)
#构造数据,包含5条虚拟数据，为39周周一周二数据，40周周四周五数据以及41周周五数据
data = data.frame(index=c('2000-09-26','2000-09-27','2000-10-26','2000-10-27','2000-11-26'),
                  Open=c(1.49,1.51,1.48,1.45,1.46),
                  High =c(1.52,1.59,1.46,1.44,1.47),
                  Low=c(0.89,0.98,0.98,0.98,0.98),
                  Close=c(1.52,1.59,1.46,1.44,1.47),
                  volatility=c(1,1,-1,-1,1),
                  weekday=c("1","2","4","5","5"),
                  weekth=c("39","39","40","40","41"))

data =xts(data[,2:8],order.by = as.POSIXct(data[,1]))

# 验证39周的数据
result = getDataInOneWeek(data,1)
#39周的周一周二有数据，其余三天为0，该周共有两条数据
expected_result = list()
length(expected_result) = 5
expected_result[[1]] = data[1,]
expected_result[[2]] = data[2,]
#then
expect_identical(result[[1]],expected_result)
expect_identical(result[[2]],2)


#验证第40周的数据
result = getDataInOneWeek(data,3)
#40周的周四周五跌，其余三天为0，该周共有两条数据
expected_result = list()
length(expected_result) = 5
expected_result[[4]] = data[3,]
expected_result[[5]] = data[4,]
#then
expect_identical(result[[1]],expected_result)
expect_identical(result[[2]],2)

#验证第41周的数据
result = getVolatilityInOneWeek(data,5)
#41周的周五涨，其余四天为0，该周共有一条数据
expected_result = list()
length(expected_result) = 5
expected_result[[5]] = data[5,]
#then
expect_identical(result[[1]],expected_result)
expect_identical(result[[2]],1)