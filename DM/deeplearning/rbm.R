require(RcppDL)
require('ltm')
data(Mobility)
data = Mobility
set.seed(2395)
n=nrow(data)
sample = sample(1:n,1000,FALSE)
data <-as.matrix(Mobility[sample ,])
n=nrow(data)
train <- sample(1:n, 800 , FALSE )
x_train <-matrix(as.numeric(unlist(data[train ,])),nrow=nrow(data[train ,]))
x_test <-matrix(as.numeric(unlist(data[-train ,])),nrow=nrow(data[-train ,]) )
x_train <-x_train[,-c(4 ,6)]
x_test <-x_test[,-c(4 ,6)]
fit <- Rrbm(x_train)
setHiddenRepresentation(fit , x = 3)
setLearningRate(fit , x = 0.01)
summary(fit)
train (fit)
reconProb <- reconstruct(fit ,x_train )
recon <-ifelse(reconProb >=0.5, 1, 0)
table(recon,x_train , dnn =c(" Predicted ", " Observed "))
image(x_train , main =" Train ")
image(recon , main =" Reconstruction ")

require(deepnet)
fit2 <-rbm.train (x_train ,hidden =3,numepochs = 3,batchsize = 100,learningrate = 0.8,learningrate_scale = 1,
                  momentum = 0.5 ,visible_type = "bin",hidden_type = "bin",cd = 1)