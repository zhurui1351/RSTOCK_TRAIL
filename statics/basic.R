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
#拒绝假设H1:U>225 ,p值大于0.05，接受原假设
x<-c(159, 280, 101, 212, 224, 379, 179, 264,222, 362, 168, 250, 149, 260, 485, 170)
#一个总体的单侧检验
result<-t.test(x,alternative="greater",mu=225)
print( result$p.value)

#两个总体的情况,p值小于0.05，拒绝原假设
X<-c(78.1,72.4,76.2,74.3,77.4,78.4,76.0,75.5,76.7,77.3)
Y<-c(79.1,81.0,77.3,79.1,80.0,79.1,79.1,77.3,80.2,82.1)
t.test(X, Y, var.equal=TRUE, alternative = "less")
#var.test可用于方差检测
var.test(X,Y)

#Pearson拟合优度
#是否均匀分布
X<-c(210, 312, 170, 85, 223)
chisq.test(X)

#是否正态分布
X<-c(25,45,50,54,55,61,64,68,72,72,75,
     78,79,81,83,84,84,84,85,86,86,86,
     87,89,89,89,90,91,91,92,100)
#分组，记数
A<-table(cut(X, br=c(0,69,79,89,100)))
#构造理论分布
p<-pnorm(c(70,80,90,100), mean(X), sd(X))
p<-c(p[1], p[2]-p[1], p[3]-p[2], 1-p[3])
#进行检测
chisq.test(A,p=p)

#回归分析
#一元线性回归

x<-c(0.10, 0.11, 0.12, 0.13, 0.14, 0.15,
     0.16, 0.17, 0.18, 0.20, 0.21, 0.23)

y<-c(42.0, 43.5, 45.0, 45.5, 45.0, 47.5,
     49.0, 53.0, 50.0, 55.0, 55.0, 60.0)

#求模型,常数项可以省略
lm.sol<-lm(y ~ x)
#查看详细信息
summary(lm.sol)
print(lm.sol)
#获取系数
coefficients(lm.sol)
#预测新的点，需使用数据框
new <- data.frame(x = 0.16)
#预测区间
lm.pred <-predict(lm.sol,new,interval = "prediction",level=0.95)
#绘制图形
plot(x,y)
abline(lm.sol)
#残差
y.res <- residuals(lm.sol)
plot(y.res)
#方差分析
#单因素方差分析
lamp<-data.frame(
  X=c(1600, 1610, 1650, 1680, 1700, 1700, 1780, 1500, 1640,
      1400, 1700, 1750, 1640, 1550, 1600, 1620, 1640, 1600,
      1740, 1800, 1510, 1520, 1530, 1570, 1640, 1600),
  A=factor(c(rep(1,7),rep(2,5), rep(3,8), rep(4,6)))
)

lamp.aov <-aov(X~A,data=lamp)
summary(lamp.aov)
#绘制图形，查看单因素的差异
plot(lamp$X~lamp$A)
#配对比较，看哪几组存在差异
pairwise.t.test(lamp$X, lamp$A, p.adjust.method = "none")
#方差齐次性检验
bartlett.test(X~A, data=lamp)

#双因素方差分析,无交互作用
agriculture<-data.frame(
  Y=c(325, 292, 316, 317, 310, 318,
      310, 320, 318, 330, 370, 365),
  A=gl(4,3),
  B=gl(3,1,12)
)

agriculture.aov <- aov(Y ~ A+B, data=agriculture)

#有交互作用
tree<-data.frame(
  Y=c(23, 25, 21, 14, 15, 20, 17, 11, 26, 21,
      16, 19, 13, 16, 24, 20, 21, 18, 27, 24,
      28, 30, 19, 17, 22, 26, 24, 21, 25, 26,
      19, 18, 19, 20, 25, 26, 26, 28, 29, 23,
      18, 15, 23, 18, 10, 21, 25, 12, 12, 22,
      19, 23, 22, 14, 13, 22, 13, 12, 22, 19),
  A=gl(3,20,60),
  B=gl(4,5,60)
)
tree.aov <- aov(Y ~ A+B+A:B, data=tree)
