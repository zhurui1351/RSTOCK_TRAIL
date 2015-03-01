#读入数据
da = read.table('C:/R/ch3data/m-unrate.txt',header = T)
#观察数据结构
head(da)
unemp = da$rate
#转换成时间序列
unrate = ts(unemp,frequency = 12,start=c(1948,1))
#绘图
plot(unrate,xlab='year',ylab='unrate',type='l')
par(mfcol=c(2,2))
#查看数据的acf pacf
acf(unemp,lag=36)
pacf(unemp,lag=36)
#查看差分的acf和pacf
acf(diff(unemp,lag=36))
pacf(diff(unemp,lag=36))
#使用arima模型建模 加入季节模式
m1 = arima(unemp,order=c(1,1,5),seasonal = list(order=c(1,0,1),period=12))
m1
c1 = c(NA,NA,NA,0,0,NA,NA,NA)
m1 = arima(unemp,order=c(1,1,5),seasonal = list(order=c(1,0,1),period=12),fixed=c1)
m1
tsdiag(m1,gof=36)
Box.test(m1$residuals,lag=24)
Box.test(m1$residuals,lag=36)

#替换模型
#使用季节为arma(1,1) 周期为12的模型
mm = arima(unemp,order=c(0,1,0),seasonal=list(order=c(1,0,1),period=12))
mm
par(mfcol=c(2,1))
acf(mm$residuals,lag=24)
pacf(mm$residuals,lag=24)
mm1 = arima(unemp,order=c(5,1,0),seasonal=list(order=c(1,0,1),period=12))
mm1
cc1 = c(0,NA,NA,NA,NA,NA,NA)
mm1 = arima(unemp,order=c(5,1,0),seasonal=list(order=c(1,0,1),period=12),fixed=cc1)
mm1
tsdiag(mm1)
#使用首次申请失业救济金人数
#注意数据时周数据，每月月数据时上月最后一周的数据

#读入数据
da = read.table('C:/R/ch3data/m-unrateic.txt',header = T)
head(da)
unrate = da$rate
x = da[,5:9] / 1000
#回归
nm1 = lm(unrate~icm1,data=x)
summary(nm1)
par(mfcol=c(2,1))
acf(mm1$residuals,lag=36)
pacf(nm1$residuals,lag=36)
nm1 = arima(unrate,order=c(2,0,3),xreg=x[,5],seasonal = list(order=c(1,0,1),period=12))
nm1
nm1 = arima(unrate,order=c(2,0,2),xreg=x[,5],seasonal = list(order=c(1,0,1),period=12))
nm1
tsdiag(nm1)

nm2 = lm(unrate~w1m1+w2m1+w3m1+w4m1,data=x)
#w3m1 w4m1不显著
summary(nm2)
nm2 = lm(unrate~w1m1+w2m1+icm1,data=x)
#icm1 不显著
summary(nm2)
nm2 = lm(unrate~w1m1+w2m1,data=x)
summary(nm2)
acf(nm2$residuals,lag=36)
pacf(nm2$residuals,lag=36)
nm2 = arima(unrate,order=c(2,0,2),seasonal = list(order=c(1,0,1),period=12),xreg=x[,1:2])
nm2
tsdiag(nm2)
