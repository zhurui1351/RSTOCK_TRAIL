library("rpart")
library(rpart.plot)
ct <- rpart.control(xval=10, minsplit=20, cp=0.1)
## rpart.control对树进行一些设置
## xval是10折交叉验证
## minsplit是最小分支节点数，这里指大于等于20，那么该节点会继续分划下去，否则停止
## minbucket：叶子节点最小样本数
## maxdepth：树的深度
## cp全称为complexity parameter，指某个点的复杂度，对每一步拆分,模型的拟合优度必须提高的程度，用来节省剪枝浪费的不必要
## kyphosis是rpart这个包自带的数据集
## na.action：缺失数据的处理办法，默认为删除因变量缺失的观测而保留自变量缺失的观测。         
## method：树的末端数据类型选择相应的变量分割方法:
## 连续性method=“anova”,离散型method=“class”,计数型method=“poisson”,生存分析型method=“exp”
## parms用来设置三个参数:先验概率、损失矩阵、分类纯度的度量方法（gini和information）
## cost是损失矩阵，在剪枝的时候，叶子节点的加权误差与父节点的误差进行比较，考虑损失矩阵的时候，从将“减少-误差”调整为“减少-损失”
fit <- rpart(Kyphosis~Age + Number + Start,data=kyphosis, method="class",control=ct,parms = list(prior = c(0.65,0.35), split = "information"));
par(mfrow=c(1,3));plot(fit); text(fit,use.n=T,all=T,cex=0.9)
rpart.plot(fit, branch=1, branch.type=2, type=1, extra=102,shadow.col="gray", box.col="green",border.col="blue", split.col="red",split.cex=1.2,main="Kyphosis决策树");
fit2 <- prune(fit, cp=0.01)
