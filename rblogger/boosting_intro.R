#http://freakonometrics.hypotheses.org/19874
#http://freakonometrics.hypotheses.org/19949
n=300
set.seed(1)
u=sort(runif(n)*2*pi)
y=sin(u)+rnorm(n)/4
df=data.frame(x=u,y=y)
plot(df)

#样条回归
v=.05 
library(splines)
fit=lm(y~bs(x,degree=1,df=3),data=df)
yp=predict(fit,newdata=df)
df$yr=df$y - v*yp
YP=v*yp

for(t in 1:100){
  fit=lm(yr~bs(x,degree=1,df=3),data=df)
  yp=predict(fit,newdata=df)
  df$yr=df$yr - v*yp
  YP=cbind(YP,v*yp)
}

nd=data.frame(x=seq(0,2*pi,by=.01))
viz=function(M){
  if(M==1)  y=YP[,1]
  if(M>1)   y=apply(YP[,1:M],1,sum)
  plot(df$x,df$y,ylab="",xlab="")
  lines(df$x,y,type="l",col="red",lwd=3)
  fit=lm(y~bs(x,degree=1,df=3),data=df)
  yp=predict(fit,newdata=nd)
  lines(nd$x,yp,type="l",col="blue",lwd=3)
  lines(nd$x,sin(nd$x),lty=2)}

viz(50)

#使用非线性spline，没有太多改进
v=.05 
fit=lm(y~bs(x,degree=2,df=3),data=df)
yp=predict(fit,newdata=df)
df$yr=df$y - v*yp
YP=v*yp
library(splines)
for(t in 1:100){
  fit=lm(yr~bs(x,degree=2,df=3),data=df)
  yp=predict(fit,newdata=df)
  df$yr=df$yr - v*yp
  YP=cbind(YP,v*yp)
}

#linear-by-parts regressio
v=.1 
idx=sample(1:n,size=n,replace=TRUE)
fit=lm(y~bs(x,degree=1,df=3),data=df[idx,])
yp=predict(fit,newdata=df)
df$yr=df$y - v*yp
YP=v*yp

for(t in 1:100){
  idx=sample(1:n,size=n,replace=TRUE)
  fit=lm(yr~bs(x,degree=1,df=3),data=df[idx,])
  yp=predict(fit,newdata=df)
  df$yr=df$yr - v*yp
  YP=cbind(YP,v*yp)
}



YP=NULL
library(splines)
for(t in 1:100){
  idx=sample(1:n,size=n,replace=TRUE)
  fit=lm(y~bs(x,degree=1,df=3),data=df[idx,])
  yp=predict(fit,newdata=nd)
  YP=cbind(YP,yp)
}
y=apply(YP[,1:100],1,mean)
plot(df$x,df$y,ylab="",xlab="")
lines(nd$x,y,type="l",col="purple",lwd=3)

#回归树
library(rpart)
v=.1 
fit=rpart(y~x,data=df)
yp=predict(fit)
df$yr=df$y - v*yp
YP=v*yp
for(t in 1:100){
  fit=rpart(yr~x,data=df)
  yp=predict(fit,newdata=df)
  df$yr=df$yr - v*yp
  YP=cbind(YP,v*yp)
}

viz=function(M){
  y=apply(YP[,1:M],1,sum)
  plot(df$x,df$y,ylab="",xlab="")
  lines(df$x,y,type="s",col="red",lwd=3)
  fit=rpart(y~x,data=df)
  yp=predict(fit,newdata=nd)
  lines(nd$x,yp,type="s",col="blue",lwd=3)
  lines(nd$x,sin(nd$x),lty=2)}
viz(50)
viz=function(v=0.05){
  fit=rpart(y~x,data=df)
  yp=predict(fit)
  df$yr=df$y - v*yp
  YP=v*yp
  for(t in 1:100){
    fit=rpart(yr~x,data=df)
    yp=predict(fit,newdata=df)
    df$yr=df$yr - v*yp
    YP=cbind(YP,v*yp)
  }
  y=apply(YP,1,sum)
  plot(df$x,df$y,xlab="",ylab="")
  lines(df$x,y,type="s",col="red",lwd=3)
  fit=rpart(y~x,data=df)
  yp=predict(fit,newdata=nd)
  lines(nd$x,yp,type="s",col="blue",lwd=3)
  lines(nd$x,sin(nd$x),lty=2)  
}




v=.1 
idx=sample(1:n,size=n,replace=TRUE)
fit=rpart(y~x,data=df[idx,])
yp=predict(fit,newdata=df)
df$yr=df$y - v*yp
YP=v*yp
for(t in 1:100){
  idx=sample(1:n,size=n,replace=TRUE)
  fit=rpart(yr~x,data=df[idx,])
  yp=predict(fit,newdata=df)
  df$yr=df$yr - v*yp
  YP=cbind(YP,v*yp)
}
