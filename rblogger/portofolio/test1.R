r1 =.06
sigma1=.12
#Asset2
r2 =.11
sigma2=.22
rho =.19
covar =rho*sigma1*sigma2
p =c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)# percent of Asset1

##Portfolio return as function of percent of Asset1##
variance=p^2*sigma1^2+(1-p)^2*sigma2^2+2*p*(1-p)*covar
sigma=sqrt(variance)
portfolio_return=r1*p+r2*(1-p)
cbind(p,sigma,return)
plot(sigma,portfolio_return,type='o')


Er=c(0.1,0.2,0.15,0.01)
S=matrix(c( .10, .01,.03, .05,
            .01, .30,.06,-.04,
            .03, .06,.40, .02,
            .05,-.04,.02, .50),nrow=4,ncol=4)

x=c(0.2,0.1,0.6,0.1)
sum(x)
x%*%Er

x=c(0.2,0.1,0.6,0.1)
S=matrix(c( .10, .01,.03, .05,
            .01, .30,.06,-.04,
            .03, .06,.40, .02,
            .05,-.04,.02, .50),nrow=4,ncol=4)
t(x)%*%S%*%x
x%*%S%*%x
#http://www.tuicool.com/articles/BzUria
library(quadprog)
library(fPortfolio)
data <- SMALLCAP.RET[, c("BKE", "GG", "GYMB", "KRON")]
# 计算得到收益率数据的协方差矩阵和期望
sigma <- covEstimator(data)$Sigma
mu <- covEstimator(data)$mu
# 计算给定期望收益率为0.03条件下的最优组合，且不可作空
A <- cbind(rep(1, 4), mu,diag(rep(1, 4))) #约束系数
D <- sigma # 协方差矩阵
x <- mu # 期望收益
b <- c(1, 0.03, 0,0,0,0) #约束的右侧值
res <- solve.QP(2 * D, x, A, b, meq=2)
round(res$solution,2)
# 设定组合的期望收益率为0.03
spec <- portfolioSpec(portfolio=list
                      (targetReturn=0.03))
# 设定组合的约束不许做空
cons <- 'LongOnly'
# 求解
res <- efficientPortfolio(data, spec = spec, 
                          constraints = cons)
summary(res)


# 设定计算出前沿上的100个最优组合，无风险资产收益率为0.01
spec <- portfolioSpec(portfolio=list
                      (nFrontierPoints = 100, 
                      riskFreeRate=0.01))
# 计算有效前沿，并不许做空
frontier <- portfolioFrontier(data,spec=spec,
                              constraints = cons)
# 有效前沿绘图
frontierPlot(frontier, 
             pch = 19,
             cex = 0.5,
             xlim=c(0,0.25),
             ylim=c(0,0.035))
grid()
abline(h = 0, col = "grey30")
abline(v = 0, col = "grey30")
minvariancePoints(frontier, pch = 19, col = "red")
tangencyPoints(frontier, pch = 19, col = "blue")
tangencyLines(frontier, col = "darkblue",lwd=3)
singleAssetPoints(frontier, pch = 19, cex = 1.5, col = topo.colors(6))
front <- frontierPoints(frontier)
monteCarloPoints(frontier, mcSteps = 500, pch = 19,
                 cex = 0.3)
lines(front, col = "red4", lwd = 3)

#####
require(quantmod) 
# 下载 QQQ/SPY/YHOO 交易数据
getSymbols(c('QQQ','SPY','YHOO')) 
# 计算收益率序列
QQQ_ret=dailyReturn(QQQ)  
SPY_ret=dailyReturn(SPY)
YHOO_ret=dailyReturn(YHOO)
dat=merge(QQQ_ret,SPY_ret,YHOO_ret)
## 转化为 timeSeries 类
require(timeSeries)
dat=as.timeSeries(dat)  
## 载入 fPortfolio
require(fPortfolio)
## 求frontier 
Frontier = portfolioFrontier(dat)
Frontier
plot(Frontier)
