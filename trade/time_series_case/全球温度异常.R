#读入温度，保存为向量
Gt = scan(file='C:/R/ch3data/m-GLBTs.txt')
#转变为时间序列
Gtemp =ts(Gt,frequency = 12,start=c(1880,1))

plot(Gtemp,xlab='year',ylab='temperature',type='l')
#acf衰减缓慢，序列相关性很强
acf(Gt,lag=100)
#查看差分的acf和pacf acf 1阶十分显著 pacf前两阶也十分显著
acf(diff(Gt),lag=36)
pacf(diff(Gt),lag=36)
#根据上面的acf和pacf建模，ar 1阶, ma 2阶 用1阶差分
m1 = arima(Gt,order=c(1,1,2))
m1
#查看残差，自相关了，因此模型本身不显著
acf(m1$residuals,lag=36)
#加入季节模式
m1 = arima(Gt,order=c(1,1,2),seasonal=list(order=c(0,0,1),period=24))
m1
#残差有所改进
acf(m1$residuals,lag=36)
tsdiag(m1,gof=36)

#一共有1568条数据
time = c(1:1568)
#回归
m2 = lm(Gt~time)
summary(m2)

par(mfcol=c(2,1))
#看趋势模型的残差
acf(m2$residuals,lag=36)
pacf(m2$residuals,lag=36)

#建模
m2 = arima(Gt,order=c(2,0,1),xreg=time)
m2
tsdiag(m2,gof=36)
#加入季节模型
m2 = arima(Gt,order=c(2,0,1),seasonal=list(order=c(0,0,1),period=24) ,xreg=time)
tsdiag(m2)
