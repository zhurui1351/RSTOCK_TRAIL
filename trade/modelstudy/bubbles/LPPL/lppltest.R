#power law分布模拟图
x=seq(1,100,by=0.5)
y=x^(-1.2)
par(mfrow=c(1,3))
plot(x,y,type='l')
plot(x,log(y),type='l')
plot(log(x),log(y),type='l')
par(mfrow=c(1,1))

#
lp = function(t,tc,A,B,C,w,o){
  y = A+B*(tc-t)^2+C*(tc-t)^2*cos(w*log((tc-t)+0i) +o)
  return(y)
}

A=7.745
B=-0.0006
C=0.000174
w=7.4
o=1.67
tc=100
t=1:200
y = sapply(t,function(x){return(lp(x,tc,A,B,C,w,o))})

plot(1:100,y[1:100])
plot(101:200,y[101:200])

#Weibull distribution x>=0
wuibullfx = function(x,z,l)
{
  return(z * ((x^z-1)/l^z) * exp(-1*(x/l)^z))
}
x=seq(1,20,by=0.1)
y=sapply(x,function(t){return(wuibullfx(t,5.5,10))})
plot(x,y)

#drawdown
x = c(1,2,1,2,1,2,1)
x = c(2,1,2,1,2,1)
localmax = findPeaks(x) - 1
localmin = findValleys(x) - 1
