library(DMwR)
data(sales)
#销售数据,insp表示分类 ok为正常销售 fraud为欺诈，unkn为未审核 
head(sales)
#探索分析
summary(sales)
#销售员个数
nlevels(sales$ID)
nlevels(sales$Prod)
length(which(is.na(sales$Quant) & is.na(sales$Val)))
sum(is.na(sales$Quant) & is.na(sales$Val))
#类型分布
table(sales$Insp)/nrow(sales)*100

totS <- table(sales$ID)
totP <- table(sales$Prod)
barplot(totS,main='Transactions per salespeople',names.arg='',xlab='Salespeople',
        ylab='Amount')
barplot(totP,main='Transactions per product',names.arg='',xlab='Products',
        ylab='Amount')

sales$Uprice <- sales$Val/sales$Quant
summary(sales$Uprice)
attach(sales)
#分产品分析价格
upp <- aggregate(Uprice,list(Prod),median,na.rm=T)
#寻找最高和最低价格的五位产品
topP <- sapply(c(T,F),function(o) upp[order(upp[,2],decreasing=o)[1:5],1])
colnames(topP) <- c('Expensive','Cheap')
topP

tops <- sales[Prod %in% topP[1,],c('Prod','Uprice')]
tops$Prod <- factor(tops$Prod)
boxplot(Uprice ~ Prod,data=tops,ylab='Uprice',log="y")
#从销售员角度分析
vs <- aggregate(Val,list(ID),sum,na.rm=T)
scoresSs <- sapply(c(T,F),function(o) 
  vs[order(vs$x,decreasing=o)[1:5],1])
colnames(scoresSs) <- c('Most','Least')
scoresSs
#前一百位销售和后两千销售分析
sum(vs[order(vs$x,decreasing=T)[1:100],2])/sum(Val,na.rm=T)*100
sum(vs[order(vs$x,decreasing=F)[1:2000],2])/sum(Val,na.rm=T)*100
#对销售产品数量进行分析
qs <- aggregate(Quant,list(Prod),sum,na.rm=T)
scoresPs <- sapply(c(T,F),function(o) 
  qs[order(qs$x,decreasing=o)[1:5],1])
colnames(scoresPs) <- c('Most','Least')
scoresPs
sum(as.double(qs[order(qs$x,decreasing=T)[1:100],2]))/
  sum(as.double(Quant),na.rm=T)*100
sum(as.double(qs[order(qs$x,decreasing=F)[1:4000],2]))/
  sum(as.double(Quant),na.rm=T)*100

#boxplot.stats(x)$out输出认为是异常点的值
out <- tapply(Uprice,list(Prod=Prod),
              function(x) length(boxplot.stats(x)$out))
out[order(out,decreasing=T)[1:10]]
sum(out)
sum(out)/nrow(sales)*100
#数据问题,删除na值数据可能导致某些产品和员工的大部分数据被删除

totS <- table(ID)
totP <- table(Prod)
nas <- sales[which(is.na(Quant) & is.na(Val)),c('ID','Prod')]

propS <- 100*table(nas$ID)/totS
propS[order(propS,decreasing=T)[1:10]]
propP <- 100*table(nas$Prod)/totP
propP[order(propP,decreasing=T)[1:10]]
#删除价格和销量同时为0的数据
detach(sales)
sales <- sales[-which(is.na(sales$Quant) & is.na(sales$Val)),]
#仅在价格或数量上有缺失的值
nnasQp <- tapply(sales$Quant,list(sales$Prod),
                 function(x) sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp,decreasing=T)[1:10]]
#删除所有数据为na的
sales <- sales[!sales$Prod %in% c('p2442','p2443'),]
nlevels(sales$Prod)
sales$Prod <- factor(sales$Prod)
nlevels(sales$Prod)


nnasQs <- tapply(sales$Quant,list(sales$ID),function(x) sum(is.na(x)))
propNAsQs <- nnasQs/table(sales$ID)
propNAsQs[order(propNAsQs,decreasing=T)[1:10]]

nnasVp <- tapply(sales$Val,list(sales$Prod),
                 function(x) sum(is.na(x)))
propNAsVp <- nnasVp/table(sales$Prod)
propNAsVp[order(propNAsVp,decreasing=T)[1:10]]


nnasVs <- tapply(sales$Val,list(sales$ID),function(x) sum(is.na(x)))
propNAsVs <- nnasVs/table(sales$ID)
propNAsVs[order(propNAsVs,decreasing=T)[1:10]]
#可用其他类似的交易来填充，比如非欺诈交易同类产品价格的中位数
tPrice <- tapply(sales[sales$Insp != 'fraud','Uprice'],
                 list(sales[sales$Insp != 'fraud','Prod']),
                 median,na.rm=T)
#填补剩余的空值
noQuant <- which(is.na(sales$Quant))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val'] /
                                    tPrice[sales[noQuant,'Prod']])
noVal <- which(is.na(sales$Val))
sales[noVal,'Val'] <- sales[noVal,'Quant'] *
  tPrice[sales[noVal,'Prod']]


sales$Uprice <- sales$Val/sales$Quant


save(sales,file='salesClean.Rdata')

#可考虑将较少的交易归并入其他类似的交易中来进行下一步处理
#使用一些统计量来对交易进行归类 ,boxplot.stats可以计算第一个四分位数和第三个四分位数
attach(sales)
notF <- which(Insp != 'fraud')
ms <- tapply(Uprice[notF],list(Prod=Prod[notF]),function(x) {
  bp <- boxplot.stats(x)$stats
  c(median=bp[3],iqr=bp[4]-bp[2])
})
ms <- matrix(unlist(ms),
             length(ms),2,
             byrow=T,dimnames=list(names(ms),c('median','iqr')))
head(ms)
#统计变量分布图
par(mfrow=c(1,2))
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='')
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='',col='grey',log="xy")
smalls <- which(table(Prod) < 20)
points(log(ms[smalls,1]),log(ms[smalls,2]),pch='+')

#kolmogorov-smirnov检验，比较任意两个样本是否来自同一分布，如果两个分布类似，那么最大距离值应该很小
#对于交易笔数小于20的品种，寻找其相似的产品


dms <- scale(ms)
smalls <- which(table(Prod) < 20)
prods <- tapply(sales$Uprice,sales$Prod,list)
similar <- matrix(NA,length(smalls),7,dimnames=list(names(smalls),
                                                    c('Simil','ks.stat','ks.p','medP','iqrP','medS','iqrS')))

for(i in seq(along=smalls)) {
  d <- scale(dms,dms[smalls[i],],FALSE)
  d <- sqrt(drop(d^2 %*% rep(1,ncol(d))))
  stat <- ks.test(prods[[smalls[i]]],prods[[order(d)[2]]])
  similar[i,] <- c(order(d)[2],stat$statistic,stat$p.value,ms[smalls[i],],
                   ms[order(d)[2],])
}
head(similar)
levels(Prod)[similar[1,1]]
nrow(similar[similar[,'ks.p'] >= 0.9,])
sum(similar[,'ks.p'] >= 0.9)
save(similar,file='similarProducts.Rdata')
