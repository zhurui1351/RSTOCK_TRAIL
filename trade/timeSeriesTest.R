#读取数据，英国42位国王年龄
kings<-scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
#1946年1月到1959年12月，纽约出生人口数据
births<-scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
#销售数据
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
#存储到时间序列中
kingstimeseries <- ts(kings)
#frequency指定一年的间隔，12指12个月
birthstimeseries<-ts(births,frequency=12,start=c(1946,1))
souvenirtimeseries<-ts(souvenir,frequency=12,start=c(1987,1))
#绘图
plot.ts(kingstimeseries )
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)
#转换模型再绘图
logsouvenirtimeseries<-log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

#分解时间序列
#分解一个时间序列意味着把它拆分成构成元件，一般序列包含一个趋势部分、一个不规则部分，如果是一个季节性时间序列，则还有一##个季节性部分。

#ttr包 使用移动平均分析趋势， 非季节成分包括趋势和不规则部分
library(TTR)
#king数据随机性类似可以用相加模型描述，使用移动平均平滑
kingstimeseriesSMA3<-SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)
#随机性仍然很强，改用8期，趋势较为明显了
kingstimeseriesSMA8<-SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

#分解季节数据，季节数据包含一个季节成分 一个趋势部分 一个不规则部分 趋势和季节部分可以用相加模型描述

birthstimeseriescomponents<-decompose(birthstimeseries)
#输出季节成分
birthstimeseriescomponents$seasonal
#画出趋势、季节、不规则部分
plot(birthstimeseriescomponents)
#去除季节影响
birthstimeseriesseasonallyadjusted<-birthstimeseries-birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
#指数平滑进行处理，前提是恒定水平以及没有季节因素的数据 测误差必须是不相关的，而且必须是服从零均值、方差不变的正态分布
#读取伦敦1813到1912年的全年降雨量
rain<-scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries<-ts(rain,start=c(1813))
#绘图,基本水平，波动也在区间内，可以使用指数平滑
plot.ts(rainseries)
rainseriesforecasts<-HoltWinters(rainseries,beta=FALSE,gamma=FALSE)
rainseriesforecasts$fitted
plot(rainseriesforecasts) 
rainseriesforecasts$SSE 
#1813年的值为初始值
HoltWinters(rainseries,beta=FALSE,gamma=FALSE,l.start=23.56)
#利用forecast进行预测
library("forecast")
rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
rainseriesforecasts2
plot.forecast(rainseriesforecasts2)
#误差的相关图
acf(rainseriesforecasts2$residuals, lag.max=20)
#检验自相关
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box") 
#残差图
plot.ts(rainseriesforecasts2$residuals)
#绘制正态分布图
plotForecastErrors <- function(forecasterrors)   {  
  # make a red histogram of the forecast errors:      
  mybinsize <- IQR(forecasterrors)/4     
  mysd   <- sd(forecasterrors)      
  mymin  <- min(forecasterrors) + mysd*5    
  mymax  <- max(forecasterrors) + mysd*3     
  mybins <- seq(mymin, mymax, mybinsize)     
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins) 
  mynorm <- rnorm(10000, mean=0, sd=mysd)    
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)      
  # plot the normal curve as a blue line on top of the histogram of forecast errors:      
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(rainseriesforecasts2$residuals) 

#霍尔特(holt)指数法 相加模型 有一个趋势 没有季节影响
#1866 到1911 裙子长度变化数据
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5) 
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)
skirtsseriesforecasts<-HoltWinters(skirtsseries,gamma=FALSE)
plot(skirtsseriesforecasts)

HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)
skirtsseriesforecasts2 <- forecast.HoltWinters(skirtsseriesforecasts, h=19) 
plot.forecast(skirtsseriesforecasts2)
acf(skirtsseriesforecasts2$residuals, lag.max=20) 
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(skirtsseriesforecasts2$residuals) 
# Holt-Winters指数平滑法 有趋势 有季节因素时使用
# 使用logsouvenirtimeseries 数据
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries) 
plot(souvenirtimeseriesforecasts) 
souvenirtimeseriesforecasts2 <- forecast.HoltWinters(souvenirtimeseriesforecasts, h=48) 
plot.forecast(souvenirtimeseriesforecasts2)
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20) 
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")

#自回归移动平均模型（ARIMA）包含一个确定（explicit）的统计模型用于处理时间序列的不规则部分，它也允许不规则部分可以自相关
#处理平稳时间序列
#如果是非平稳序列，进行差分处理，得到稳定的序列，ARIMA(p,d,q)模型，其中d是差分的阶数。
#可以使用diff()做差分
#使用裙子数据
skirtsseriesdiff1 <- diff(skirtsseries, differences=1) 
plot.ts(skirtsseriesdiff1)
#仍不稳定，做二阶差分
skirtsseriesdiff2 <- diff(skirtsseries, differences=2) 
plot.ts(skirtsseriesdiff2)
#可以使用fUnitRoots进行平稳性检测

kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1)
#求自相关图
acf(kingtimeseriesdiff1, lag.max=20) 
acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) 
pacf(kingtimeseriesdiff1, lag.max=20)
pacf(kingtimeseriesdiff1, lag.max=20,plot=FALSE)
#auto.arima() 可以用来发现合适的arma模型
auto.arima(kings)
#1500 - 1969 北半球 火山灰覆盖数据
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500)) 
plot.ts(volcanodustseries)
acf(volcanodustseries, lag.max=20)
acf(volcanodustseries, lag.max=20,plot=FALSE)

pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20,plot=FALSE)

kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1))
kingstimeseriesarima

getDifferData <- function(init,c=1,a=0.5,n=500,mean=0,var=0.5)
{
  y = c(init)
  for(i in 2 : n)
  {
    m = y[i-1]
    n = c + a*m + rnorm(1,mean,var)
    y= c(y,n)
  }
  return(y)
}

y = getDifferData(3,a=1,n=1000)

plot(y,type='l')
abline(a=mean(y),b=0)
acf(y)
pacf(y)
Box.test(y, lag=30, type="Ljung-Box") 


getMaData <- function(c=1,a=0.5,n=500,mean=0,var=0.5)
{
  r = rnorm(n,mean,var)
  y = c()
  for(i in 2 : n)
  {
    n = c + a*r[i-1]+ r[i]
    y= c(y,n)
  }
  return(y)
}
m=getMaData()
plot(m,type='l')
acf(m)

Box.test(m, lag=1, type="Ljung-Box") 


#案例研究
da = read.table('C:/R/ch3data/w-petroprice.txt',header = T)
da1 = read.table('C:/R/ch3data/w-gasoline.txt')
#使用对数价格序列
pgs = log(da1[,1])
pus = log(da$US)
#日期时间
tdx = c(1:717) / 52 + 1997
par(mfcol=c(2,1))
#绘图查看数据
plot(tdx,pgs,xlab='year',ylab='ln(price)',type='l')
title(main='（a） Gosolin')
plot(tdx,pus,xlab='year',ylab='ln(price)',type='l')
title(main='（b） Crude oil')
#差分，也就是增长率
dpgs=diff(pgs)
#acf和pacf图可以看出有ar的特征
acf(dpgs)
pacf(dpgs)
#用ar建模
m1 = ar(diff(pgs),method='mle')
#查看模型的阶数
m1$order
#增长率并不显著与0，因此ar的mean可以视为0，不带常数项
t.test(dpgs)
#重新用arima做模型，去掉常数项
m1 = arima(dpgs,order=c(5,0,0),include.mean = F)
m1
#4阶t比小于1，可以去掉
m1 = arima(dpgs,order=c(5,0,0),include.mean = F,fixed=c(NA,NA,NA,0,NA))
m1

dpus = diff(pus)
#两者差分的线性关系
m3 = lm(dpgs ~ -1 +dpus)
summary(m3)
#检验残差,残差有序列关系
acf(m3$residuals,lag=20)
pacf(m3$residuals,lag=20)
m4 = ar(m3$residuals,method='mle')
m4$order
#建模
m4 = arima(dpgs,order=c(6,0,0),include.mean = F,xreg = dpus)
m4
#用5阶模型
m4 = arima(dpgs,order=c(5,0,0),include.mean = F,xreg = dpus)
m4 = arima(dpgs,order=c(5,0,0),include.mean = F,xreg = dpus,fixed = c(NA,NA,NA,0,NA,NA))
tsdiag(m4,gof=20)
