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
tsdiag(m1,gof=20)

dpus = diff(pus)
#两者差分的线性关系
m3 = lm(dpgs ~ -1 +dpus)
summary(m3)
#检验残差,残差有序列关系
acf(m3$residuals,lag=20)
pacf(m3$residuals,lag=20)
#对残差建模
m4 = ar(m3$residuals,method='mle')
m4$order
#建立线性模型
m4 = arima(dpgs,order=c(6,0,0),include.mean = F,xreg = dpus)
m4
#用5阶模型
m4 = arima(dpgs,order=c(5,0,0),include.mean = F,xreg = dpus)
m4 = arima(dpgs,order=c(5,0,0),include.mean = F,xreg = dpus,fixed = c(NA,NA,NA,0,NA,NA))
tsdiag(m4,gof=20)
#样本外测试
c1 = c(NA,NA,NA,0,NA)
pm1 = backtest(m1,dpgs,316,1,fixed=c1,inc.mean=F)
c4 = c(NA,NA,NA,0,NA,NA)
pm4 = backtest(m4,dpgs,316,1,xre=dpus,inc.mean=F,fixed=c4)

tdx = tdx[2:717]
pm4fit = dpgs[317:716] - pm4$error
pm1fit = dpgs[317:716] - pm1$error
plot(tdx[317:716],dpgs[317:716],xlab='year',ylab='growth',type='l')
points(tdx[317:716],pm1fit,pch='*')
plot(tdx[317:716],dpgs[317:716],xlab='year',ylab='growth',type='l')
points(tdx[317:716],pm4fit,pch='*')
#提前三天的模型
m6 = lm(dpgs[2:716] ~ -1 +dpus[1:715])
summary(m6)
acf(m6$residuals,lag=20)
pacf(m6$residuals,lag=20)
m7 = ar(m6$residuals,methond='mle')
m7$order
m7 = arima(dpgs[2:716],order=c(9,0,0),include.mean=F,xreg=dpus[1:715])
m7
m7 = arima(dpgs[2:716],order=c(9,0,0),include.mean=F,xreg=dpus[1:715],
           fixed=c(NA,NA,NA,0,NA,0,0,0,NA,NA))
tsdiag(m7,gof=20)
c7 = c(NA,NA,NA,0,NA,0,0,0,NA,NA)
pm7 = backtest(m7,dpgs[2:716],315,1,xre=dpus[1:715],inc.mean=F,fixed=c7)
