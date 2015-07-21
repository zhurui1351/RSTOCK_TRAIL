#http://f.dataguru.cn/thread-266479-1-1.html

file = 'D:/Rcode/code/RSTOCK_TRAIL/rblogger/data/bank/bank-full.csv'
bank <- read.csv(file, sep = ";", header = T)
head(bank)
summary(bank)
#运用决策树模型对数据做初步分类建模和变量选择

require(rpart)
require(caret)
require(ggplot2)
require(gplots)
bank.tree <- rpart(y ~ ., data = bank, method = "class", cp = 0.001)
treeImp <- varImp(bank.tree, scale = TRUE, surrogates = FALSE, competes = TRUE)
treeImp$Variable <- rownames(treeImp)
treeImp.sort <- treeImp[order(-treeImp$Overall), ]
ggplot(treeImp, aes(Variable, Overall)) + geom_bar(stat = "identity") + coord_flip()
#剪枝后的误差图
plotcp(bank.tree)
printcp(bank.tree)


bank.tree <- rpart(y ~ ., data = bank, method = "class", cp = 0.0022373)
#绘制决策树
plot(bank.tree, branch = 0, margin = 0.1, uniform = T)
text(bank.tree, use.n = T, col = "red", cex = 0.6)

#选择重要性最高的变量进行分析


bank$y_dummy = ifelse(bank$y == "yes", 1, 0)
summary(bank$duration)
ggplot(bank, aes(duration, y_dummy)) + geom_smooth() + geom_point()
#做一次二次项
bank$duration.sq <- bank$duration * bank$duration
summary(bank$month)
bank$month.sel <- ifelse(bank$month == "dec", 1, 0)
bank$month.sel <- ifelse(bank$month == "mar", 1, bank$month)
bank$month.sel <- ifelse(bank$month == "oct", 1, bank$month)
bank$month.sel <- ifelse(bank$month == "sep", 1, bank$month)
summary(bank$poutcome)
bank$poutcome.success <- ifelse(bank$poutcome == "success", 1, 0)
summary(bank$pdays)
bank$nocontact <- ifelse(bank$pdays == -1, 1, 0)
bank$pdays <- ifelse(bank$pdays == -1, 0, bank$pdays)
summary(bank$nocontact)
ggplot(bank, aes(log(pdays + 1))) + geom_histogram()
ggplot(bank, aes(log(pdays + 1), y_dummy)) + geom_smooth() + geom_point()
summary(bank$previous)
ggplot(bank, aes(log(previous + 1))) + geom_histogram()
bank$previous.0 <- as.factor(ifelse(bank$previous == 0, 1, 0))
bank$previous.1 <- as.factor(ifelse(bank$previous == 1, 1, 0))
bank$previous.2 <- as.factor(ifelse(bank$previous == 2, 1, 0))
bank$previous.2plus <- as.factor(ifelse(bank$previous > 2, 1, 0))
#逻辑回归
logistic.full <- glm(y_dummy ~ duration + duration.sq + month.sel + poutcome.success + 
                       bank$nocontact + log(pdays + 1) + bank$previous.0 + bank$previous.1 + bank$previous.2 + 
                       bank$previous.2plus, data = bank)
summary(logistic.full)

logistic.step <- step(logistic.full, direction = "both", k = 2)
summary(logistic.step)

#模型得分和评估

require(ROCR)
bank.pred<-1/(1+exp(-predict(logistic.step)))
roc.data <- prediction(bank.pred, labels = bank$y)
roc.data <- performance(roc.data, "tpr", "fpr")
plot(roc.data)


score<-data.frame("prob.y"=bank.pred,"y"=as.factor(bank$y_dummy))
ggplot(score, aes(x=prob.y, fill=y)) + geom_histogram(position="identity", binwidth=0.01,alpha=0.5)
