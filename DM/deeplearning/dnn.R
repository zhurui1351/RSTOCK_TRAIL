require(deepnet)
require(RSNNS)
require(neuralnet)
require(Metrics)
set.seed(2016)
attribute <- as.data.frame(sample(seq(-2,2, length =50),50, replace = FALSE),ncol =1)
response <-attribute^2
data <- cbind(attribute,response)
colnames(data) = c("attribute","response")
fit = neuralnet(response~attribute,data=data,hidden = c(3,3),threshold=0.01)
testdata <- as.matrix(sample(seq(-2,2,length =10),10,replace = FALSE), ncol =1)
pred<-compute(fit,testdata)
attributes(pred)
result <- cbind(testdata,pred$net.result,testdata ^2)
colnames(result) <- c("Attribute","Prediction", "Actual")
round(result,4)
##############################
data("Boston",package ="MASS")
data <-Boston
keeps <- c("crim", "indus", "nox", "rm" ,"age", "dis", "tax" ,"ptratio", "lstat","medv")
data = data[keeps]
f<-medv~ crim + indus + nox + rm + age + dis + tax + ptratio + lstat
set.seed(2006)
n=nrow(data)
train <- sample (1:n, 400 , FALSE )
fit <- neuralnet(f,data=data[train,],hidden =c(20,12,20),algorithm="rprop+",stepmax=1e+05,
                 err.fct = "sse",act.fct = "logistic",threshold =0.5,linear.output=T)
pred<-compute(fit,data[-train,1:9])
round(cor(pred$net.result,data[-train ,10]))
mse(data[-train ,10],pred$net.result)
plot(fit)

#deepnet
X= as.matrix(data[train,1:9])
Y= data[train,10]
fitB <-nn.train(x=X, y=Y,initW =NULL,initB = NULL,hidden = c(10 ,12 ,20),learningrate = 0.58,
                  momentum =0.74,learningrate_scale =1,activationfun = "sigm",output = "linear",
                  numepochs = 970,batchsize = 60,hidden_dropout=0,visible_dropout=0)
Xtest <- data[-train ,1:9]
predB <- nn.predict(fitB , Xtest )
round(cor(predB$net.result,data[-train,10])^2,6)
mse(data[-train ,10],predB)
rmse(data[-train ,10],predB)

######
data("PimaIndiansDiabetes2",package ="mlbench")
str(PimaIndiansDiabetes2)
sapply(PimaIndiansDiabetes2,function(x){sum(is.na(x))})
temp <-(PimaIndiansDiabetes2)
temp$insulin = NULL
temp$triceps = NULL
temp = na.omit(temp)
y<-(temp$diabetes)
temp$diabetes = NULL
temp = scale(temp)
temp = cbind(as.factor(y),temp)
colnames(temp)[1] = 'V1'
set.seed(2016)
n = nrow(temp)
n_train=600
n_test = n - n_train
train <- sample (1:n, n_train , FALSE )
require(RSNNS)
X = temp[train,2:7]
Y = temp[train,1]
fitMLP <- mlp(x=X, y=Y,size = c(12 ,8),maxit = 1000,initFunc="Randomize_Weights",initFuncParams = c(-0.3 , 0.3),
               learnFunc="Std_Backpropagation",learnFuncParams=c(0.2,0),updateFunc="Topological_Order",
               updateFuncParams=c(0),hiddenActFunc = "Act_ Logistic",shufflePatterns=TRUE,linOut=TRUE)
predMLP=sign(predict(fitMLP,temp[-train,2:7]))
table(predMLP ,sign(temp[-train ,1]) ,dnn =c("Predicted","Observed"))
####
detach ("package:RSNNS",unload =TRUE)
require(AMORE)
net<-newff(n.neurons =c(6 ,12 ,8 ,1) ,learning.rate.global =0.01,momentum.global=0.5,
          error.criterium="LMLS",Stao =NA,hidden.layer="sigmoid",output.layer ="purelin",
              method ="ADAPTgdwm")
fit <- train(net,P=X,T=Y,error.criterium ="LMLS",report=TRUE,show.step =100,n.shows =5 )
pred <- sign( sim (fit$net , temp[-train ,]) )
table(pred ,sign(temp[-train ,1]) ,dnn =c("Predicted","Observed"))
######多响应变量
data("bodyfat",package ="TH.data")
require(neuralnet)
require(Metrics)
set.seed(2016)
train = sample(1:71,50,F)
scale_bodyfat <-as.data.frame(scale(log(bodyfat)))
f= waistcirc + hipcirc ~ DEXfat +age + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4
fit <- neuralnet(f,data = scale_bodyfat[train ,],hidden =c(8,4),threshold =0.1,stepmax=1e+6,
                err.fct = "sse",algorithm="rprop+",act.fct="logistic",linear.output = FALSE)
without_fat <-scale_bodyfat
without_fat$waistcirc <-NULL
without_fat$hipcirc <-NULL
pred<-compute(fit , without_fat[-train ,])
pred$net.result


fw = waistcirc~ DEXfat +age + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4
fh = hipcirc ~ DEXfat +age + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4
regw <-linReg <-lm(fw ,data = scale_bodyfat[train ,])
regh <-linReg <-lm(fh ,data = scale_bodyfat[train ,])
predw <-predict (regw , without_fat[-train,])
predh <-predict (regh , without_fat[-train,])
mse(scale_bodyfat[-train ,10],pred$net.result[ ,1])
mse(scale_bodyfat[-train ,10],predw)
mse(scale_bodyfat[-train ,10],pred$net.result[ ,2])
mse(scale_bodyfat[-train ,10],predh)
X= as.matrix(without_fat[train,])
Y= as.matrix(scale_bodyfat[train ,3:4])
fitB <-nn.train(x=X, y=Y,initW=NULL,initB=NULL,hidden=c(8,4),activationfun="sigm",learningrate=0.02,
                  momentum =0.74 ,learningrate_scale =1,output="linear",numepochs=970,batchsize=60,
                  hidden_dropout=0.2,visible_dropout = 0)
Xtest<-as.matrix(without_fat[-train ,])
predB <- nn.predict(fitB , Xtest)
mse(scale_bodyfat[-train ,10],predB[ ,1])
mse(scale_bodyfat[-train ,10],predB[ ,2])
