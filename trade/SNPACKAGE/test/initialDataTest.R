require(testthat)
require(quantmod)

#构造测试数据
data = data.frame(index=c('2000-09-26','2000-09-27'),Open=c(1.49,1.51),High =c(1.52,1.59),
                  Low=c(0.89,0.98),Close=c(1.50,1.56)
                  )
#非xts对象，报错
expect_error(initialData(data))

data =xts(data[,2:5],order.by = as.POSIXct(data[,1]))

data = initialData(data)
#断言新增三列的值
expect_identical("1",as.character(data[1,'volatility']))
expect_identical("2",as.character(data[1,'weekday']))
expect_identical("39",as.character(data[1,'weekth']))

