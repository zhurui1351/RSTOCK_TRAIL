da = read.table('C:/R/ch4data/m-intcsp7309.txt',header=T)

head(da)
#求对数增长率
intc = log(da$intc + 1)
#绘图
rtn = ts(intc,frequency = 12,start=c(1973,1))
plot(rtn,type='l',xlab='year',ylab='ln-rtn')
#t测试 是否显著异于0
t.test(intc)
#测试序列相关性,基本不相关
Box.test(intc,lag=12,type='Ljung')

par(mfcol=c(2,1))
acf(intc,lag=24)
acf(abs(intc),lag=24)
#绝对值相关，从而序列不独立
Box.test(abs(intc),lag=12,type='Ljung')

#建立均值方程，问简单平均值
y = intc - mean(intc)
#用序列平方做ARCH效应检验
Box.test(y^2,lag=12,type='Ljung')
#拉格朗日测试
source('C:/mac/desk/R/code/trade/time_series_case/archTest.R', echo=TRUE)
archTest(y,12)
