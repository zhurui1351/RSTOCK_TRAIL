#来源 数据挖掘与R语言
library('car')
library('DMwR')
head(algae)
#查看一下统计特征
summary(algae)
hist(algae$mxPH,prob=T)
par(mfrow=c(1,2))
hist(algae$mxPH,prob=T,ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))
#查看 qq图，看是否满足正态分布，有异常点，并不完全符合正态
qq.plot(algae$mxPH)
par(mfrow=c(1,1))
#箱型图
boxplot(algae$oPO4)
rug(jitter(algae$oPO4),side=2)
abline(h=mean(algae$oPO4,na.rm=T),lty=2)
#画图识别异常点
plot(algae$NH4,xlab="")
abline(h=mean(algae$NH4,na.rm=T),lty=1)
abline(h=mean(algae$NH4,na.rm=T) +sd(algae$NH4,na.rm=T),lty=2)
abline(h=median(algae$NH4,na.rm=T),lty=3)
#交互
identify(algae$NH4)
#条件box图
require('lattice')
bwplot(size~a1,data=algae)
#拉箱图
require(Hmisc)
bwplot(size~a1,data=algae,panel=panel.bpplot,probs=seq(.01,.49,by=0.1),datadensity=T)
#处理缺失数据，1.剔除案例 2.根据变量相似性填补 3.根据案例相似性填补 4.选用能够处理缺失数据的工具
nrow(algae[!complete.cases(algae),])
na.omit(algae)
#返回每行数据的缺失值个数
apply(algae,1,function(x) sum(is.na(x)))
manyNAs(algae)
algae[-manyNAs(algae),]
#使用中位数填补值
algae[is.na(algae$Chla),'Chla'] <- median(algae$Chla,na.rm=T)
data(algae)
algae <- algae[-manyNAs(algae),]
#填补缺失值
algae <- centralImputation(algae)
#使用相关性较高的变量填补缺失值
#求相关系数矩阵,忽略na值
s=cor(algae[,4:18],use="complete.obs")
#获得每个变量相关系数最高的配对
result = list(NULL)
index = 1
for(i in 1 : ncol(s))
{
  x = s[-i,i]
  name = names(which(x==max(x)))
  v = max(x)
  result[[index]] = list(var=colnames(s)[i],corvar = name,cor = v)
  index = index + 1
}
symnum(cor(algae[,4:18],use="complete.obs"))
#oPO4和PO4的相关性很高 可以建立回归模型填充缺失值
f=lm(PO4~oPO4,data=algae)
#填补28行的数据
fillOp = function(op,f){return(f$coefficients[1] + f$coefficients[2] * op)}
algae[28,'PO4'] = f$coefficients[1] + f$coefficients[2] * algae[28,'oPO4']
algae[is.na(algae$PO4),"PO4"] = sapply(algae[is.na(algae$PO4),"oPO4"],fillOp)
#不同季节的直方图
histogram(~mxPH | season,data=algae)
#不同河流和大小组合下的值
histogram(~mxPH | size * speed,data=algae)

#通过案例相似性来填充数据,使用欧式距离定义相似性，选取最相似的10个案例，用中位数来填补缺失值
#名义变量则用众数，也可以用加权均值等
data(algae)
algae <- algae[-manyNAs(algae),]
algae = knnImputation(algae,k=10)
#预测，7种海藻的出现频率
#1.多元线性回归,对于名义变量，会生成k-1个辅助变量
data(algae)
algae <- algae[-manyNAs(algae),]
clean.algae =  knnImputation(algae,k=10)
lm.a1 = lm(a1~.,data = clean.algae[,1:12])
summary(lm.a1)
#初始拟合并不显著,使用方差分析简化模型
anova(lm.a1)
#可见season的p值最大，贡献最小，剔除该变量
lm2.a1 = update(lm.a1,.~.-season)
summary(lm2.a1)
#R平方改善对比,不显著
anova(lm.a1,lm2.a1)
#使用向后校元得到新模型,R平方仍然不理想，因此一般的线性模型不一定有效
final.lm = step(lm.a1)
summary(final.lm)
#使用回归树，回归树本身能处理缺失数据
require(rpart)
data(algae)
algae <- algae[-manyNAs(algae),]
rt.a1 <- rpart(a1~.,data=algae[,1:12])
#可视化回归树
plot(rt.a1)
text(rt.a1)
prettyTree(rt.a1)
#剪枝
printcp(rt.a1)
rt2.a1 <- prune(rt.a1,cp=0.08)

#模型评估
lm.predictions.a1 = predict(final.lm,clean.algae)
rt.predictions.a1 = predict(rt.a1,algae)

#计算误差 先计算平均绝对误差MAE 然后计算均方误差 MSE
mae.a1.lm  = mean(abs(lm.predictions.a1- clean.algae[,'a1']))
mae.a1.rt  = mean(abs(rt.predictions.a1- algae[,'a1']))

mse.a1.lm = mean((lm.predictions.a1- clean.algae[,'a1'])^2)
mse.a1.rt = mean((rt.predictions.a1- algae[,'a1'])^2)

#标准化绝对误差NMSE 值越小，性能越好
nmse.a1.lm = mean((lm.predictions.a1- clean.algae[,'a1'])^2) / mean((mean(clean.algae[,'a1'])- clean.algae[,'a1'])^2)
nmse.a1.rt = mean((rt.predictions.a1- algae[,'a1'])^2) / mean((mean(algae[,'a1'])- algae[,'a1'])^2)
#绘制误差点图,理论上预测较好的模型应该落在直线周围
old.par=par(mfrow=c(1,2))
plot(lm.predictions.a1,algae[,'a1'])
abline(0,1,lty=2)
plot(rt.predictions.a1,algae[,'a1'])
abline(0,1,lty=2)
par(old.par)

#频率不该为负值,修正模型
sensible.lm.predictions.a1 = ifelse(lm.predictions.a1 < 0,0,lm.predictions.a1)
regr.eval(algae[,'a1'],sensible.lm.predictions.a1)
regr.eval(algae[,'a1'],lm.predictions.a1)
#k-cross validation

cv.rpart = function(form,train,test,...)
{
  m = rpartXse(form,train,...)
  p = predict(m,test)
  mse = mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

cv.lm = function(form,train,test,...)
{
  m = lm(form,train,...)
  p = predict(m,test)
  p = ifelse(p<0,0,p)
  mse = mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

#交叉验证比较
res = experimentalComparison(
  c(dataset(a1~.,clean.algae[,1:12],'a1')),
  c(variants('cv.lm'),variants('cv.rpart',se=c(0,0.5,1))),
  cvSettings(3,10,1234)
  )

summary(res)
plot(res)
getVariant("cv.rpart.v1",res)
#比较7个预测任务
DSs = sapply(names(clean.algae)[12:18],
             function(x,names.attrs)
             {
               f = as.formula(paste(x,"~ ."))
               dataset(f,clean.algae[,c(names.attrs,x)],x)
             },
             names(clean.algae[1:11])
             )

res.all = experimentalComparison(DSs,c(variants('cv.lm'),variants('cv.rpart',se=c(0,0.5,1))),
                                 cvSettings(5,10,1234))
plot(res.all)
#寻找最优模型
bestScores(res.all)
#可见除a1外效果都不是很好，使用随机森林
library(randomForest)
