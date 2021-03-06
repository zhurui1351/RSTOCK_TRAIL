require(TSA)
data("larain")
data("color")
data("hare")
data("tempdub")
data("oilfilters")
data("rwalk")
model1 = lm(rwalk ~ time(rwalk))
summary(model1)
win.graph(width = 4.875,height = 2.5,pointsize = 8)
plot(rwalk,type='o',ylab='y')
abline(model1)

month. = season(tempdub)
model2 = lm(tempdub~month.)
summary(model2)
har. = harmonic(tempdub,1)
model3 = lm(tempdub~har.)

win.graph(width = 4.875,height = 2.5,pointsize = 8)
data("ar1.s")
plot(ar1.s,ylab=expression(Y[t]),type='o')
plot(y=ar1.s,x=zlag(ar1.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

data("ma1.1.s")
win.graph(width = 4.875,height = 2.5,pointsize = 8)
acf(ma1.1.s,xaxp=c(0,20,10))
acf(ma1.1.s,ci.type='ma',xaxp=c(0,20,10))
data("ma1.2.s")
acf(ma1.2.s,xaxp=c(0,20,10))
data("ma2.s")
acf(ma2.s,xaxp=c(0,20,10))
acf(ma2.s,ci.type='ma',xaxp=c(0,20,10))

data("ar1.s")
acf(ar1.s,xaxp=c(0,20,10))
pacf(ar1.s,xaxp=c(0,20,10))
data("ar2.s")
acf(ar2.s,xaxp=c(0,20,10))
pacf(ar2.s,xaxp=c(0,20,10))
data("arma11.s")
plot(arma11.s,type='o',ylab=expression(Y[t]))
acf(arma11.s,xaxp=c(0,20,10))
pacf(arma11.s,xaxp=c(0,20,10))
eacf(arma11.s)
#非平稳序列
data("oil.price")
acf(oil.price,xaxp=c(0,24,12))
pacf(oil.price,xaxp=c(0,24,12))
acf(diff(as.vector(log(oil.price))),xaxp=c(0,24,12))

data("rwalk")
acf(diff(rwalk,differences = 2),ci.type='ma',xaxp=c(0,18,9))
acf(diff(rwalk),ci.type='ma',xaxp=c(0,18,9))

#单位根检测 区分随机非平稳和趋势非平稳
ar(diff(rwalk))
require(uroot)
adf.test(rwalk,k=0)
adf.test(diff(rwalk),k=8)
#
set.seed(92397)
test = arima.sim(model=list(ar=c(rep(0,11),.8),ma=c(rep(0,11),.7)),n=120)
res = armasubsets(y=test,nar=14,nma=14,y.name='test',ar.method = 'ols')
plot(res)

#真实数据序列识别
data("larain")
qqnorm(log(larain))
qqline(log(larain))
acf(log(larain),xaxp=c(0,20,10))
#不具有明显的相关性，作为正太分布处理

data("ma1.1.s")
arima(ma1.1.s,order = c(0,0,1),method = 'CSS',include.mean = T)
data("ar2.s")
ar(ar2.s,order.max = 2,AIC=F,method='yw')
ar(ar1.s,order.max = 2,AIC=F,method='ols')
ar(ar1.s,order.max = 2,AIC=F,method='mle')
#残差图
data(color)
m1.color = arima(color,order=c(1,0,0))
plot(rstandard(m1.color),ylab='standardized residuals',type='o')
abline(h=0)
qqnorm(rstandard(m1.color))
qqline(rstandard(m1.color))
acf(rstandard(m1.color),plot=F)$acf
signif(acf(rstandard(m1.color),plot=F)$acf,2)
#ljung-box 残差检测
tsdiag(m1.color,gof=15,omit.initial = F)
#过渡拟合诊断，可以看到系数，标准差的差异，选择更简单的模型
arima(color,order=c(1,0,0))
arima(color,order=c(2,0,0))
arima(color,order=c(1,0,1))
arima(color,order=c(2,0,1))
#预测
data(color)
m1.color = arima(color,order=c(1,0,0))
plot(m1.color,n.ahead=12,type='b',xlab='time',ylab='color property')
abline(h=coef(m1.color)[names(coef(m1.color))=='intercept'])
#季节模型  随机季节模型
data(co2)
plot(co2,ylab='co2')

x = 1
for(i in 1 : 1000)
{
  y = x[i] + rnorm(1) 
  x=c(x,y)
}

#参数拟合
data(ar1.s)
data("ar2.s")
ar(ar1.s,order.max = 1,AIC=F,method = 'yw')
ar(ar1.s,order.max = 1,AIC=F,method = 'ols')
ar(ar1.s,order.max = 1,AIC=F,method = 'mle')

ar(ar2.s,order.max = 2,AIC=F,method = 'mle')
ar(ar2.s,order.max = 2,AIC=F,method = 'ols')
ar(ar2.s,order.max = 2,AIC=F,method = 'yw')

data("arma11.s")
arima(arma11.s,order=c(1,0,1),method = 'CSS')

#季节差分
plot(co2)
acf(as.vector(co2),lag.max = 36)
plot(diff(co2))
acf(as.vector(diff(co2)),lag.max = 36)
plot(diff(diff(co2),lag=12))
acf(as.vector(diff(diff(co2),lag=12)),lag.max = 36,ci.type='ma')
m1.co2 = arima(co2,order=c(0,1,1),seasonal = list(order=c(0,1,1),period = 12))
m1.co2
plot(rstandard(m1.co2),type='o')
abline(h=0)
acf(as.vector(rstandard(m1.co2)),lag.max = 36)
qqnorm(as.vector(rstandard(m1.co2)))
qqline(as.vector(rstandard(m1.co2)))
#过渡拟合模型
m2.co2 = arima(co2,order=c(0,1,2),seasonal = list(order=c(0,1,1),period = 12))
#预测
plot(m1.co2,n1=c(1990,1),n.ahead=24,xlab='year',type='o',ylab='co2 level')
plot(m1.co2,n1=c(1990,1),n.ahead=48,xlab='year',type='o',ylab='co2 level')

data("airmiles")
plot(log(airmiles))
