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

