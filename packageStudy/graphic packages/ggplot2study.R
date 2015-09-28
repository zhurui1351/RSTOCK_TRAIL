#R数据可视化手册 ggplot2数据分析与图形艺术

library(ggplot2)
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds),100),]
dsmall
#quick plot qplot
qplot(carat,price,data=diamonds)
#对变量进行变换
qplot(log(carat),log(price),data=diamonds)
#体积和重量之间的关系
qplot(carat,x*y*z,data=diamonds)
#关系图中加入颜色和切工信息，而在plot里面需要手工绘制细节
qplot(carat,price,data=dsmall,color=color)
qplot(carat,price,data=dsmall,color=color)
qplot(carat,price,data=dsmall,shape=cut)
qplot(carat,price,data=dsmall,shape=cut,color=color)
#设置透明度
qplot(carat,price,data=diamonds,alpha=I(1/10))
qplot(carat,price,data=diamonds,alpha=I(1/100))
