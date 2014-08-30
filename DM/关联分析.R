buy<-data.frame(面包=c('1','1','0','1','1'),牛奶=c('1','0','1','1','1'),尿布=c('0','1','1','1','1'),啤酒=c('0','1','1','1','0'),鸡蛋=c('0','1','0','0','0'),可乐=c('0','0','1','0','1'))
rules <- apriori(buy,parameter = list(minlen=2, supp=0.5, conf=0.8))
#泰坦尼克例子
load("~/Desktop/R/titanic.raw.rdata")
#船舱等级，性别，年龄， 年龄（是个分类变量包含成人和儿童）， 是否幸存
str(titanic.raw)
#加载关联算法的程序包
library(arules)
#生成规则
rules <- apriori(titanic.raw)
#检查所返回的关联规则
inspect(rules)
#如果只想检查其它变量和乘客是否幸存的关系，那么需要提前设置变量rhs=c("Survived=No", "Survived=Yes")
# 现在生成的关联规则结果只包含("Survived=No", "Survived=Yes")
rules <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8),
                  appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"), control = list(verbose=F))
#根据关联结果中的提升度(life)进行降序排序
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
#生成一个关联规则的子集矩阵，
subset.matrix <- is.subset(rules.sorted, rules.sorted)
#将矩阵对角线以下的元素置为空
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
#将子集矩阵中每列元素和大于等于1的列找出来
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
#从规则矩阵中去掉这些列
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))

