#http://site.douban.com/182577/widget/notes/12817482/note/273585095/
# 加载扩展包和数据
library(caret)
data(PimaIndiansDiabetes2,package='mlbench')
# 对缺失值使用装袋方法进行插补
preproc <- preProcess(PimaIndiansDiabetes2[-9],method="bagImpute")
data <- predict(preproc,PimaIndiansDiabetes2[-9])
data$Class <- PimaIndiansDiabetes2[,9]
# 使用朴素贝叶斯建模，这里使用了三次10折交叉检验得到30个结果
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
model1 <- train(Class~., data=data,method='nb',trControl = fitControl,tuneGrid = data.frame(.fL=1,.usekernel=F))
# 观察30次检验结果，发现准确率在0.75左右
resampleHist(model1)
# 返回训练数据的混淆矩阵
pre <- predict(model1)
confusionMatrix(pre,data$Class)

# 加载包
library(bnlearn)
# 数据离散化
data2 <- discretize(data[-9],method='quantile')
data2$class <- data[,9]
# 使用爬山算法进行结构学习
bayesnet <- hc(data2)
# 显示网络图
plot(bayesnet)
# 修改网络图中的箭头指向
bayesnet<- set.arc(bayesnet,'age','pregnant')
# 参数学习
fitted <- bn.fit(bayesnet, data2,method='mle')
# 训练样本预测并提取混淆矩阵
pre <- predict(fitted,data=data2,node='class')
confusionMatrix(pre,data2$class)
# 进行条件推理
cpquery(fitted,(class=='pos'),(age=='(36,81]'&mass=='(34.8,67.1]'))



#exercise运动量
#acid高饱和脂肪酸摄入量
#family家族病史
#hc高胆固醇
#hp高血脂
#smoke吸烟
#ca心脏病
#各自视为2值的随机变量（大小，高低，有无）

source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
biocLite("RBGL")

library("gRain")
library("Rgraphviz")
#指定条件概率表
yn <- c("yes", "no")
e <- cptable(~ exercise, values = c(4, 6), levels = yn)
a<- cptable(~ acid, values = c(3, 7), levels = yn)
f<- cptable(~ family, values = c(2, 8), levels = yn)
hc.e <- cptable(~ hc + exercise, values = c(1, 99, 5, 95), levels = yn)
s<- cptable(~ smoke, values = c(5,5), levels = yn)
ca.s <- cptable(~ ca + smoke, values = c(6, 4, 3, 7), levels = yn)
ca.f <- cptable(~ ca + family, values = c(6, 4, 3, 7), levels = yn)
ca.e <- cptable(~ ca + exercise, values = c(1, 99, 5, 95), levels = yn)
ca.hc <- cptable(~ ca + hc, values = c(80, 20, 30, 70), levels = yn)
hc.a <- cptable(~ hc + acid, values = c(80, 20, 30, 70), levels = yn)
hp.a <- cptable(~ hp + acid, values = c(80, 20, 30, 70), levels = yn)
ca.hp <- cptable(~ ca + hp, values = c(80, 20, 30, 70), levels = yn)
hc.f <- cptable(~ hc + family, values = c(6, 4, 3, 7), levels = yn)
hp.f <- cptable(~ hp + family, values = c(6, 4, 3, 7), levels = yn)

#条件概率表（CPT）
plist <- compileCPT(list(e,a,s,f,hc.e,ca.s,ca.f,ca.e,ca.hc,hc.a,hp.a,ca.hp,hc.f,hp.f))

plist
plist[5]

#从CRT创建网络
gin1 <- grain(plist)
summary(gin1)

#画出网络图
plot(gin1)

library(bnlearn)
library(gRain)
# 利用条件概率表生成网络
asianet <- model2network("[asia][smoke][tub|asia][lung|smoke][bronc|smoke][either|tub:lung][xray|either][dysp|bronc:either]")
yn <- c("yes", "no")
cptA <- matrix(c(0.01, 0.99), ncol = 2, dimnames = list(NULL, yn))
cptS <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, yn))
cptT <- matrix(c(0.05, 0.95, 0.01, 0.99), ncol = 2, dimnames = list(tub = yn, 
                                                                    asia = yn))
cptL <- matrix(c(0.1, 0.9, 0.01, 0.99), ncol = 2, dimnames = list(lung = yn, 
                                                                  smoke = yn))
cptB <- matrix(c(0.6, 0.4, 0.3, 0.7), ncol = 2, dimnames = list(bronc = yn, 
                                                                smoke = yn))
cptE <- c(1, 0, 1, 0, 1, 0, 0, 1)
dim(cptE) <- c(2, 2, 2)
dimnames(cptE) <- list(either = yn, lung = yn, tub = yn)
cptX <- matrix(c(0.98, 0.02, 0.05, 0.95), ncol = 2, dimnames = list(xray = yn, 
                                                                    either = yn))
cptD <- c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9)
dim(cptD) <- c(2, 2, 2)
dimnames(cptD) <- list(dysp = yn, either = yn, bronc = yn)
asianet.fit <- custom.fit(asianet, dist = list(asia = cptA, smoke = cptS, tub = cptT, 
                                               lung = cptL, bronc = cptB, either = cptE, xray = cptX, dysp = cptD))

plot(asianet)
# moral图
asianet.m <- moral(asianet)
plot(asianet.m)
# 精确查询
asia.jtree <- compile(as.grain(asianet.fit))
summary(asia.jtree)
asia.ev1 <- setFinding(asia.jtree, nodes = c("asia", "dysp"), states = c("yes", 
                                                                         "yes")) #设定证据
querygrain(asia.ev1, nodes = "lung", type = "marginal") #在证据之下查询
querygrain(asia.jtree, nodes = "lung", type = "marginal") #无证据下的查询
# 近似查询,接上面的程序 给定证据，查询事件发生的概率
set.seed(1000)
cpquery(asianet.fit, event = (lung == "yes"), evidence = (asia == "yes") & (dysp == 
                                                                              "yes"))
lung.sample <- cpdist(asianet.fit, nodes = "lung", evidence = (asia == "yes") & 
                        (dysp == "yes"))
prop.table(table(lung.sample))
marks.bn <- gs(marks, undirected = FALSE)
marks.bn
marks.bn <- gs(marks, debug = TRUE) 
graphviz.plot(marks.bn, layout = "fdp")
blacklisting <- data.frame(c("STAT", "STAT", "ALG"), c("ANL", "ALG", "MECH"))
marks.bn2 <- gs(marks, blacklist = blacklisting)
highlight.opts <- list(nodes = c("STAT", "ANL"), arcs = c("ANL", "STAT"), col = "blue", 
                       fill = "grey", lwd = 2.5)
graphviz.plot(marks.bn2, layout = "fdp", highlight = highlight.opts)
