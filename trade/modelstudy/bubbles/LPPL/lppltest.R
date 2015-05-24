#power law分布模拟图
x=seq(1,100,by=0.5)
y=x^(-1.2)
par(mfrow=c(1,3))
plot(x,y,type='l')
plot(x,log(y),type='l')
plot(log(x),log(y),type='l')
par(mfrow=c(1,1))
