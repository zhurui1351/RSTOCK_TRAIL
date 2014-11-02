x1<-c(35,40,40,42,37,35,45,43,37)
mean(x1)
sd(x1)
min(x1)
max(x1)
range(x1)
which.min(x1)
which.max(x1)
sum(x1)
prod(x1)
length(x1)
median(x1)
var(x1)
sort(x1)
quantile(x1)
cv <- 100*sd(x1)/mean(x1); 
css <- sum((x1-mean(x1))^2)
data_outline <- function(x){
  n <- length(x)
  m <- mean(x)
  v <- var(x)
  s <- sd(x)
  me <- median(x)
  cv <- 100*s/m
  css <- sum((x-m)^2)
  uss <- sum(x^2)
  R <- max(x)-min(x)
  R1 <- quantile(x,3/4)-quantile(x,1/4)
  sm <- s/sqrt(n)
  g1 <- n/((n-1)*(n-2))*sum((x-m)^3)/s^3
  g2 <- ((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/s^4-(3*(n-1)^2)/((n-2)*(n-3))) 
  data.frame(N=n,Mean=m,Var=v,std_dev=s,Media=me,std_mean=sm,CV=cv,CSS=css,USS=uss,R=R,R1=R1,Skewness=g1,Kurtosis=g2,row.names=1)
}
pnorm(1,0,1)
dnorm(1,0,1)
qnorm(1-0.025,0,1)
boxplot(x1)
fivenum(x1)
shapiro.test(x1)

interval_estimate1<-function(x,sigma=-1,alpha=0.05){
  n<-length(x);
  xb<-mean(x);
  if(sigma>=0)
  {
    tmp <- sigma/sqrt(n) * qnorm(1-alpha/2);df<-n
  }
  else{
    tmp<-sd(x)/sqrt(n)*qt(1-alpha/2,n-1);df<-n-1
  }
  data.frame(mean = xb,df=df,a=xb-tmp,b=xb+tmp)
}
t.test(x1)
interval_estimate1(x1)
                             
P_value<-function(cdf, x, paramet=numeric(0), side=0){
  n<-length(paramet)
  P<-switch(n+1,
            cdf(x),
            cdf(x, paramet),
            cdf(x, paramet[1], paramet[2]),
            cdf(x, paramet[1], paramet[2], paramet[3])
  )
  if (side<0) P
  else if (side>0) 1-P
  else
    if (P<1/2) 2*P
  else 2*(1-P)
} 

mean.test1<-function(x, mu=0, sigma=-1, side=0){
  n<-length(x); xb<-mean(x)
  if (sigma>0){
    z<-(xb-mu)/(sigma/sqrt(n))
    P<-P_value(pnorm, z, side=side)
    data.frame(mean=xb, df=n, Z=z, P_value=P)
  }
  else{
    t<-(xb-mu)/(sd(x)/sqrt(n))
    P<-P_value(pt, t, paramet=n-1, side=side)
    data.frame(mean=xb, df=n-1, T=t, P_value=P)
  }}


mean.test2<-function(x, y,
                     sigma=c(-1, -1), var.equal=FALSE, side=0){
  source("P_value.R")
  n1<-length(x); n2<-length(y)
  xb<-mean(x); yb<-mean(y)
  if (all(sigma>0)){
    z<-(xb-yb)/sqrt(sigma[1]^2/n1+sigma[2]^2/n2)
    P<-P_value(pnorm, z, side=side)
    data.frame(mean=xb-yb, df=n1+n2, Z=z, P_value=P)
  }
  else{
    if (var.equal == TRUE){
      Sw<-sqrt(((n1-1)*var(x)+(n2-1)*var(y))/(n1+n2-2))
      t<-(xb-yb)/(Sw*sqrt(1/n1+1/n2))
      nu<-n1+n2-2
    }
    else{
      S1<-var(x); S2<-var(y)
      nu<-(S1/n1+S2/n2)^2/(S1^2/n1^2/(n1-1)+S2^2/n2^2/(n2-1))
      t<-(xb-yb)/sqrt(S1/n1+S2/n2)
    }
    P<-P_value(pt, t, paramet=nu, side=side)
    data.frame(mean=xb-yb, df=nu, T=t, P_value=P)
  }
} 

#can use t.test to test


#var.test可用于方差假设估计