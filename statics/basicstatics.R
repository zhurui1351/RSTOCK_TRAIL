#以mtcars数据为例介绍基本的描述性统计和统计推断的方法
#miles per gallon (mpg), horsepower (hp), and weight (wt)
vars <- c("mpg", "hp", "wt")
head(mtcars[vars])
summary(mtcars[vars])

mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}

sapply(mtcars[vars], mystats)
library(Hmisc)
describe(mtcars[vars])
#分组聚合
aggregate(mtcars[vars], by=list(am=mtcars$am), mean)
aggregate(mtcars[vars], by=list(am=mtcars$am), sd)
#aggregate只能单值，by可以多值
dstats <- function(x)(c(mean=mean(x), sd=sd(x)))
by(mtcars[vars], mtcars$am, dstats)
library(doBy)
summaryBy(mpg+hp+wt~am, data=mtcars, FUN=mystats)
library(psych)
describeBy(mtcars[vars], mtcars$am)
library(reshape)
dstats <- function(x)(c(n=length(x), mean=mean(x), sd=sd(x)))
dfm <- melt(mtcars, measure.vars=c("mpg", "hp", "wt"),
            id.vars=c("am", "cyl"))
cast(dfm, am + cyl + variable ~ ., dstats)
#频率分析
library(vcd)
head(Arthritis)
mytable <- with(Arthritis, table(Improved))
mytable
#百分比形式
prop.table(mytable)
prop.table(mytable)*100
#联列表
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable
#按行汇总
margin.table(mytable, 1)
prop.table(mytable, 1)
#按列汇总
margin.table(mytable, 2)
prop.table(mytable, 2)
#行列汇总
addmargins(mytable)
addmargins(prop.table(mytable))

library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)
#多维联列表
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
mytable
ftable(mytable)
margin.table(mytable, c(1, 3))
ftable(prop.table(mytable, c(1, 2)))

#Tests of independence chi test fisher test Cochran-Mantel–Haenszel test
library(vcd)
#是否使用药物治疗和疗效的关系是不独立的
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
#这两个变量不相互独立
chisq.test(mytable)
#性别同治疗效果的关系 是独立的
mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)
#FISHER’S EXACT TEST
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)
#Cochran–Mantel–Haenszel 在性别控制下，疗效不独立
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)

#联列表的相联性 association
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)
#Correlations 相关性 度量线性
states<- state.x77[,1:6]
cov(states)
cor(states)
cor(states, method="spearman")
x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)
#偏相关
library(ggm)
pcor(c(1,5,2,3,6), cov(states))
#相关性的显著性分析
cor.test(states[,3], states[,5])
library(psych)
corr.test(states, use="complete")
#t检验
library(MASS)
t.test(Prob ~ So, data=UScrime)
sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime, t.test(U1, U2, paired=TRUE))
#组间差异的非参分析
#Mann–Whitney U test
with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data=UScrime)

#回归分析
#简单回归
fit <- lm(weight ~ height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in pounds)")
abline(fit)
#多项式回归
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)

plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))

fit3 <- lm(weight ~ height + I(height^2) +I(height^3), data=women)
#点图
library(car)
scatterplot(weight ~ height,
            data=women,
            spread=FALSE, lty.smooth=2,
            pch=19,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")

#多个变量
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
cor(states)
#两两分析
scatterplotMatrix(states, spread=FALSE, lty.smooth=2,
                  main="Scatter Plot Matrix")
#假设变量相互独立
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,data=states)
summary(fit)
#变量之间有交互作用
fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)
library(effects)
plot(effect("hp:wt", fit,list(wt=c(2.2,3.2,4.2))),multiline=TRUE)

 fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
#置信区间
confint(fit)
#回归诊断
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
#残差 qq图等
plot(fit)

fit2 <- lm(weight ~ height + I(height^2), data=women)
par(mfrow=c(2,2))
plot(fit2)
#增强包 car
library(car)
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fit, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
#异常点分析 Nevada"
states["Nevada",]
fitted(fit)["Nevada"]
residuals(fit)["Nevada"]
rstudent(fit)["Nevada"]
#残差与正态分布图比较
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit)
#Durbin–Watson测试
durbinWatsonTest(fit)
#partial residual plots 分析变量独立性
crPlots(fit)
#constant variance检测
ncvTest(fit)
spreadLevelPlot(fit)

#Global validation of linear model assumption
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)
#多重共线性 可以使用variance inflation factor检测
vif(fit)
sqrt(vif(fit)) > 2
#异常值
outlierTest(fit)
#高杠杆值点
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)
#强影响点
#Cook’s D values检验
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
#Added-variable plots 方法
avPlots(fit, ask=FALSE, onepage=TRUE, id.method="identify")
influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook’s distance")
#观察到不完全满足条件后的调整方法
#1.删除观察值，除非有证据表明数据记录有误或其他 否则最好不要随便删除数据
#2.对变量进行转化 比如进行n次方转化 或 如果是比例数据可以用[ln (Y/1-Y)]
#对n次方的估计
library(car)
#可见可以用muder的0.6次方进行转化
summary(powerTransform(states$Murder))
#对线性性的改进可以使用如下函数
boxTidwell(Murder~Population+Illiteracy,data=states)
#3.添加或删除变量
#4.使用其他回归方法
#寻找最佳模型
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
#比较模型1和2，看新增的变量是否显著
anova(fit2, fit1)
#AIC
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
AIC(fit1,fit2)
#变量选择
#逐步回归
library(MASS)
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
stepAIC(fit1, direction="backward")
#全子集回归
library(leaps)
leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                     Frost, data=states, nbest=4)
plot(leaps, scale="adjr2")
library(car)
subsets(leaps, statistic="cp",
        main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")

#k交叉验证
shrinkage <- function(fit, k=10){
  require(bootstrap)
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}

fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states)
shrinkage(fit)
fit2 <- lm(Murder~Population+Illiteracy,data=states)
shrinkage(fit2)

#标准化的回归
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
coef(zfit)
#相对重要性
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg=lbls,
          ylab="% of R-Square",
          xlab="Predictor Variables",
          main="Relative Importance of Predictor Variables",
          sub=paste("R-Square=", round(rsquare, digits=3)),
          ...)
  return(import)
  fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
}
relweights(fit, col="lightgrey")
#方差分析
library(multcomp)
head(cholesterol)
attach(cholesterol)
table(trt)
#各组均值
aggregate(response,by=list(trt),FUN=mean)
#各组标准差
aggregate(response,by=list(trt),FUN=sd)
#组间差异
fit = aov(response~trt)
summary(fit)
#绘制各组均值与置信区间
library(gplots)
plotmeans(response~trt,xlab='Treatment',ylab='Response',main="mean plot 95% ci")
detach(cholesterol)
#防止兼容性问题
detach("package::HH")
#各组均值成对比较
TukeyHSD(fit)
par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit))

library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(trt="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")

#方差分析假设因变量服从正态分布，各组方差相等。可以使用Q-Q图来检验正态性假设
library(car)
qqPlot(lm(response ~ trt, data=cholesterol),
       simulate=TRUE, main="Q-Q Plot", labels=FALSE)
#方差齐次检测
bartlett.test(response ~ trt, data=cholesterol)
#离群点
outlierTest(fit)

#单因素协方差分析
data(litter, package="multcomp")
head(litter)
attach(litter)
table(dose)
aggregate(weight, by=list(dose), FUN=mean)
fit <- aov(weight ~ gesttime + dose)
summary(fit)
library('effects')
effect("dose",fit)
#多重比较
contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
summary(glht(fit, linfct=mcp(dose=contrast)))

fit2 <- aov(weight ~ gesttime*dose, data=litter)
summary(fit2)
library(HH)
ancova(weight ~ gesttime + dose, data=litter)
detach(litter)
#双因素方差分析
attach(ToothGrowth)
table(supp, dose)
aggregate(len, by=list(supp, dose), FUN=mean)
aggregate(len, by=list(supp, dose), FUN=sd)
fit <- aov(len ~ supp*dose)
summary(fit)
interaction.plot(dose, supp, len, type="b",
                 col=c("red","blue"), pch=c(16, 18),
                 main = "Interaction between Dose and Supplement Type")
require(gplots)
plotmeans(len ~ interaction(supp, dose, sep=" "),
          connect=list(c(1,3,5),c(2,4,6)),
          col=c("red", "darkgreen"),
          main = "Interaction Plot with 95% CIs",
          xlab="Treatment and Dose Combination")
interaction2wt(len~supp*dose)

#重复测量方差分析

237
