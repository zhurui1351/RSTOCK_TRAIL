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
173