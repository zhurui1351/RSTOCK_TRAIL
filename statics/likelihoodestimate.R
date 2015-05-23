#正态分布的似然函数，theta是指需要估计的正态分布的均值和方差，logL是似然值，
#之所以返回负数是因为后面用到的优化似然值的函数是最小化函数。
normal <- function(theta,x){
  mu <- theta[1]
  sigma2 <- theta[2]
  n <- length(x)
  logL <- -0.5*n*log(2*pi)-0.5*n*log(sigma2)-(1/(2*sigma2))*sum((x-mu)**2)
  return (-logL)
}

#函数optim()可以进行后续的优化
#optim(initial valurs of theta, likelihood function,data)
x <- rnorm(100)
result <- optim(c(0,1),normal,x=x)

#使用maxLik
require(maxLik)
x <- rnorm(100,1,2)
N <- length(x)
normal <- function(theta){
  mu <- theta[1]
  sigma <- theta[2]
  logL <- -0.5*N*log(2*pi) - N*log(sigma) - sum(0.5*(x - mu)^2/sigma^2)
  return (logL)
}

result <- maxLik(normal,start=c(0,1))

