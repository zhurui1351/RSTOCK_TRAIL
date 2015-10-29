#http://site.douban.com/182577/widget/notes/10567181/note/278503359/
n<-500
diet<-0.1#通过控制饮食而恢复正常的
effect<-c(0,0.95)
names(effect)<-c('FA','FB')
#对病患总体指派因素F
f.chance<-runif(n)
f<-ifelse(f.chance<0.9,'FA','FB')
#指派控制组和治疗组
group<-runif(n)
group<-ifelse(group<0.5,'control','drug')
#指派治疗结果
diet.chance<-runif(n)
drug.chance<-runif(n)
outcome<-((diet.chance<diet)|(drug.chance<effect[f]*(group=='drug')))

trail<-data.frame(group=group,F=f,treatment=outcome)

#看看模拟的结果：

summary(trail)
with(trail[group=='control',],table(F,treatment))
with(trail[group=='drug',],table(F,treatment))
#分组的治疗效果
treat.group<-with(trail,table(group,treatment))
#频率派 进行显著性检测
chisq.test(treat.group)
#贝叶斯
library(ggplot2)
p<-ggplot(data.frame(x=c(0,1)),aes(x=x))
p+stat_function(fun=dbeta,args=list(0.1,0.9),colour='red')
#利用公式计算Beta分布的均值和众数
betad.mean<-function(alpha,beta)
{alpha/(alpha+beta)}
betad.mode<-function(alpha,beta)
{(alpha+1)/(alpha+beta-2)}
#先验分布的情况
alpha<-0.1
beta<-0.9
#分控制组和治疗组分别讨论
false.control<-treat.group[1,1]
true.control<-treat.group[1,2]
false.drug<-treat.group[2,1]
true.drug<-treat.group[2,2]
#对控制组，后验分布的参数
alpha.control<-alpha+true.control
beta.control<-beta+false.control
#对治疗组，后验分布的参数
alpha.drug<-alpha+true.drug
beta.drug<-beta+false.drug
#画出这两个后验分布
p<-ggplot(data.frame(x=c(0,.3)),aes(x=x))
p+stat_function(fun=dbeta,args=list(alpha.drug,beta.drug),colour='red')+
  stat_function(fun=dbeta,args=list(alpha.control,beta.control),colour='blue')+
  annotate("text",x=.03,y=20,label="control")+
  annotate("text",x=.23,y=15,label="drug")
#计算控制组的均值和众数（p的后验估计）

betad.mean(alpha.control,beta.control)
betad.mode(alpha.control,beta.control)
#计算治疗组的均值和众数(p的后验估计)

betad.mean(alpha.drug,beta.drug)
betad.mode(alpha.drug,beta.drug)


#从1960年代贝叶斯统计学派复兴到今天贝叶斯统计的广泛运用，MCMC方法起到了极重要
#的作用。作为一种计算手段，MCMC以模拟的方法解决了贝叶斯方法中后验分布的计算问题。
library(MCMCpack)
data(swiss)
swiss.posterior1 <- MCMCregress(Fertility ~ Agriculture + Examination + Education + 
                                  Catholic + Infant.Mortality, data = swiss)
summary(swiss.posterior1)
#对比最小二乘回归

swiss.lm <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + 
                 Infant.Mortality, data = swiss)
summary(swiss.lm)

plot(swiss.posterior1[, 1:2], col = 4)

#贝叶斯因子选择、模型选择
swiss.posterior1 <- MCMCregress(Fertility ~ Agriculture + Examination + Education + 
                                  Catholic + Infant.Mortality, data = swiss, marginal.likelihood = "Chib95", 
                                b0 = 0, B0 = 0.1, c0 = 2, d0 = 0.11)

swiss.posterior2 <- MCMCregress(Fertility ~ Agriculture + Education + Catholic + 
                                  Infant.Mortality, data = swiss, marginal.likelihood = "Chib95", b0 = 0, 
                                B0 = 0.1, c0 = 2, d0 = 0.11)

swiss.posterior3 <- MCMCregress(Fertility ~ Agriculture + Examination + Catholic + 
                                  Infant.Mortality, data = swiss, marginal.likelihood = "Chib95", b0 = 0, 
                                B0 = 0.1, c0 = 2, d0 = 0.11)
bf <- BayesFactor(swiss.posterior1, swiss.posterior2, swiss.posterior3)
