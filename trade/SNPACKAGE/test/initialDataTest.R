require(testthat)
require(quantmod)

#构造测试数据
data = data.frame(index=c('2000-09-26','2000-09-27','2000-10-02','2000-10-03','2000-11-09'),Open=c(1.49,1.51,1.46,1.47,1.5),High =c(1.52,1.59,1.55,1.56,1.5),
                  Low=c(0.89,0.98,1,1.1,1.5),Close=c(1.50,1.56,1.51,1.56,1.5)
                  )
#非xts对象，报错
expect_error(initialData(data))

data =xts(data[,2:5],order.by = as.POSIXct(data[,1]))
#周内数据
data_w = initialData(data)
#断言新增三列的值
expect_identical("1",as.character(data_w[1,'volatility']))
expect_identical("2",as.character(data_w[1,'sec']))
expect_identical("39",as.character(data_w[1,'fst']))

#周间数据
data_m = initialData(data,type='bw')
#断言新增三列的值
expect_identical("1",as.character(data_m[1,'volatility']))
expect_identical("1",as.character(data_m[1,'sec']))
expect_identical("09",as.character(data_m[1,'fst']))

#月间数据
data_bm = initialData(data,type='bm')
#断言新增三列的值
expect_identical("1",as.character(data_bm[1,'volatility']))
expect_identical("09",as.character(data_bm[1,'sec']))
expect_identical("2000",as.character(data_bm[1,'fst']))
