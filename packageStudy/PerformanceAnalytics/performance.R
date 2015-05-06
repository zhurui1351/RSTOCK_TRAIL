testdata = data.frame(index=c('2000-09-26','2000-09-27','2000-09-28','2000-09-29','2000-09-30'),
                  Ra=c(1.49,1.51,1.48,1.45,1.46),
                  Rb =c(1,1,1,1,1))

testdata =xts(testdata[,2:3],order.by = as.POSIXct(testdata[,1]),tz="GMT")

#默认scala是252，也就是日收益率
Return.annualized(testdata[,'Ra']，testdata[,'Rb']))

#默认scala是252，也就是日收益率
Return.annualized(testdata[,'Ra'])

#从头开始 累积调用函数
apply.fromstart(testdata[,'Ra'], FUN="mean",gap=3)
#按照移动时间窗口进行计算，和上面的apply.fromstart有区别
apply.rolling(testdata[,'Ra'],width=2,FUN="mean")
