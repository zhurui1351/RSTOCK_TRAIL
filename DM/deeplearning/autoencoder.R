require(autoencoder)
require(ripa)
data(logo)
image(logo)
logo
x_train = t(logo)
set.seed(2016)
fit <-autoencode(X.train =x_train,X.test=NULL,nl=3,N.hidden=60,unit.type="logistic",lambda=1e-5,
                beta=1e-5,rho =0.3,epsilon =0.1,max.iterations = 100,optim.method=c("BFGS"),
                rel.tol=0.01,rescale.flag = TRUE,rescaling.offset=0.001)
attributes( fit )
fit$mean.error.training.set
features <- predict (fit, X.input =x_train,hidden.output = TRUE )
image(t(features$X.output))
pred <- predict(fit , X.input =x_train,hidden.output = FALSE )
pred$mean.error
recon <-pred$X.output
image(t(recon))
###############################################

aburl='http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
names = c('sex','length','diameter','height','whole.weight','shucked.weight',
           'viscera.weight','shell.weight','rings')
data = read.table(aburl,header = F,sep = ',',col.names=names)
summary(data)
data[data$ height ==0 ,]
data$ height[data$height ==0] = NA
data <-na.omit(data)
data$sex <-NULL
summary(data)
data1 <-t(data)
data1 <-as.matrix(data1)
set.seed(2016)
n=nrow(data)
train <- sample(1:n, 10, FALSE )
fit <-autoencode(X.train =data1[,train],X.test=NULL,nl=3,N.hidden=5,unit.type="logistic",lambda=1e-5,
                 beta=1e-5,rho =0.07,epsilon =0.1,max.iterations = 100,optim.method=c("BFGS"),
                 rel.tol=0.01,rescale.flag = TRUE,rescaling.offset=0.001)
fit$mean.error.training.set
features <- predict(fit,X.input=data1[,train], hidden.output = TRUE )
features$X.output
pred <- predict(fit,X.input=data1[,train], hidden.output = FALSE )
#Stacked Autoencoder
require(SAENET)
data = read.table(aburl,header = F,sep = ',',col.names=names)
summary(data)
data[data$ height ==0 ,]
data$ height[data$height ==0] = NA
data <-na.omit(data)
data$sex <-NULL
data1 <-as.matrix(data)
set.seed(2016)
n=nrow(data)
train <- sample(1:n, 10, FALSE )
fit <-SAENET.train(X.train=data1[train,],n.nodes=c(5,4,2),unit.type="logistic",lambda=1e-5,
                  beta=1e-5,rho = 0.07,epsilon =0.1,max.iterations = 100,optim.method=c("BFGS"),
                  rel.tol=0.01,rescale.flag=TRUE,rescaling.offset=0.001)

fit[[3]]$X.output
#Denoising autoencoder
require(RcppDL)
require("ltm")
data( Mobility)



################################################################################
##                                                                            ##
##                     Training an Auto-encoder in R                          ##
##                                                                            ##
################################################################################


## https://www.kaggle.com/c/digit-recognizer

## data and H2O setup
digits.train <- read.csv("train.csv")
digits.train$label <- factor(digits.train$label, levels = 0:9)

cl <- h2o.init(
  max_mem_size = "12G",
  nthreads = 4)

h2odigits <- as.h2o(
  digits.train,
  destination_frame = "h2odigits")

i <- 1:20000
h2odigits.train <- h2odigits[i, -1]

itest <- 20001:30000
h2odigits.test <- h2odigits[itest, -1]
xnames <- colnames(h2odigits.train)

m1 <- h2o.deeplearning(
  x = xnames,
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "Tanh",
  autoencoder = TRUE,
  hidden = c(50),
  epochs = 20,
  sparsity_beta = 0,
  input_dropout_ratio = 0,
  hidden_dropout_ratios = c(0),
  l1 = 0,
  l2 = 0
)

m2a <- h2o.deeplearning(
  x = xnames,
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "Tanh",
  autoencoder = TRUE,
  hidden = c(100),
  epochs = 20,
  sparsity_beta = 0,
  input_dropout_ratio = 0,
  hidden_dropout_ratios = c(0),
  l1 = 0,
  l2 = 0
)

m2b <- h2o.deeplearning(
  x = xnames,
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "Tanh",
  autoencoder = TRUE,
  hidden = c(100),
  epochs = 20,
  sparsity_beta = .5,
  input_dropout_ratio = 0,
  hidden_dropout_ratios = c(0),
  l1 = 0,
  l2 = 0
)

m2c <- h2o.deeplearning(
  x = xnames,
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "Tanh",
  autoencoder = TRUE,
  hidden = c(100),
  epochs = 20,
  sparsity_beta = 0,
  input_dropout_ratio = .2,
  hidden_dropout_ratios = c(0),
  l1 = 0,
  l2 = 0
)

m1
m2a
m2b
m2c


error1 <- as.data.frame(h2o.anomaly(m1, h2odigits.train))
error2a <- as.data.frame(h2o.anomaly(m2a, h2odigits.train))
error2b <- as.data.frame(h2o.anomaly(m2b, h2odigits.train))
error2c <- as.data.frame(h2o.anomaly(m2c, h2odigits.train))

error <- as.data.table(rbind(
  cbind.data.frame(Model = 1, error1),
  cbind.data.frame(Model = "2a", error2a),
  cbind.data.frame(Model = "2b", error2b),
  cbind.data.frame(Model = "2c", error2c)))

percentile <- error[, .(
  Percentile = quantile(Reconstruction.MSE, probs = .99)
), by = Model]

p <- ggplot(error, aes(Reconstruction.MSE)) +
  geom_histogram(binwidth = .001, fill = "grey50") +
  geom_vline(aes(xintercept = Percentile), data = percentile, linetype = 2) +
  theme_bw() +
  facet_wrap(~Model)
print(p)

png("../FirstDraft/chapter04_images/B4228_04_03.png",
    width = 5.5, height = 5.5, units = "in", res = 600)
print(p)
dev.off()

error.tmp <- cbind(error1, error2a, error2b, error2c)
colnames(error.tmp) <- c("M1", "M2a", "M2b", "M2c")
plot(error.tmp)

png("../FirstDraft/chapter04_images/B4228_04_04.png",
    width = 5.5, height = 5.5, units = "in", res = 600)
plot(error.tmp)
dev.off()


features1 <- as.data.frame(h2o.deepfeatures(m1, h2odigits.train))
r.features1 <- cor(features1)
r.features1 <- data.frame(r = r.features1[upper.tri(r.features1)])

p.hist <- ggplot(r.features1, aes(r)) +
  geom_histogram(binwidth = .02) +
  theme_classic()
print(p.hist)

png("../FirstDraft/chapter04_images/B4228_04_05.png",
    width = 5, height = 5, units = "in", res = 600)
print(p.hist)
dev.off()



m3 <- h2o.deeplearning(
  x = xnames,
  training_frame= h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "Tanh",
  autoencoder = TRUE,
  hidden = c(100, 10),
  epochs = 30,
  sparsity_beta = 0,
  input_dropout_ratio = 0,
  hidden_dropout_ratios = c(0, 0),
  l1 = 0,
  l2 = 0
)


features3 <- as.data.frame(h2o.deepfeatures(m3, h2odigits.train, 2))
head(features3)

features3$label <- digits.train$label[i]
features3 <- melt(features3, id.vars = "label")

p.line <- ggplot(features3, aes(as.numeric(variable), value,
                                colour = label, linetype = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  scale_x_continuous("Deep Features", breaks = 1:10) +
  theme_classic() +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
print(p.line)

png("../FirstDraft/chapter04_images/B4228_04_06.png",
    width = 7, height = 6, units = "in", res = 600)
print(p.line)
dev.off()


################################################################################
##                                                                            ##
##                                 Use Case                                   ##
##                                                                            ##
################################################################################


use.train.x <- read.table("UCI HAR Dataset/train/X_train.txt")
use.test.x <- read.table("UCI HAR Dataset/test/X_test.txt")

use.train.y <- read.table("UCI HAR Dataset/train/y_train.txt")[[1]]
use.test.y <- read.table("UCI HAR Dataset/test/y_test.txt")[[1]]

use.labels <- read.table("UCI HAR Dataset/activity_labels.txt")

h2oactivity.train <- as.h2o(
  use.train.x,
  destination_frame = "h2oactivitytrain")

h2oactivity.test <- as.h2o(
  use.test.x,
  destination_frame = "h2oactivitytest")


mu1 <- h2o.deeplearning(
  x = colnames(h2oactivity.train),
  training_frame= h2oactivity.train,
  validation_frame = h2oactivity.test,
  activation = "Tanh",
  autoencoder = TRUE,
  hidden = c(100, 100),
  epochs = 30,
  sparsity_beta = 0,
  input_dropout_ratio = 0,
  hidden_dropout_ratios = c(0, 0),
  l1 = 0,
  l2 = 0
)

erroru1 <- as.data.frame(h2o.anomaly(mu1, h2oactivity.train))

pue1 <- ggplot(erroru1, aes(Reconstruction.MSE)) +
  geom_histogram(binwidth = .001, fill = "grey50") +
  geom_vline(xintercept = quantile(erroru1[[1]], probs = .99), linetype = 2) +
  theme_bw()
print(pue1)

png("../FirstDraft/chapter04_images/B4228_04_07.png",
    width = 5.5, height = 5.5, units = "in", res = 600)
print(pue1)
dev.off()

i.anomolous <- erroru1$Reconstruction.MSE >= quantile(erroru1[[1]], probs = .99)

pu.anomolous <- ggplot(as.data.frame(table(use.labels$V2[use.train.y[i.anomolous]])),
                       aes(Var1, Freq)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
print(pu.anomolous)

png("../FirstDraft/chapter04_images/B4228_04_08.png",
    width = 5.5, height = 5.5, units = "in", res = 600)
print(pu.anomolous)
dev.off()


################################################################################
##                                                                            ##
##                       Fine tuning an Auto-encoder                          ##
##                                                                            ##
################################################################################


## create 5 folds
require(data.table)
require(caret)
set.seed(1234)
folds <- createFolds(1:20000, k = 5)

## create parameters to try
hyperparams <- list(
  list(
    hidden = c(50),
    input_dr = c(0),
    hidden_dr = c(0)),
  list(
    hidden = c(200),
    input_dr = c(.2),
    hidden_dr = c(0)),
  list(
    hidden = c(400),
    input_dr = c(.2),
    hidden_dr = c(0)),
  list(
    hidden = c(400),
    input_dr = c(.2),
    hidden_dr = c(.5)),
  list(
    hidden = c(400, 200),
    input_dr = c(.2),
    hidden_dr = c(.25, .25)),
  list(
    hidden = c(400, 200),
    input_dr = c(.2),
    hidden_dr = c(.5, .25)))

fm <- lapply(hyperparams, function(v) {
  lapply(folds, function(i) {
    h2o.deeplearning(
      x = xnames,
      training_frame = h2odigits.train[-i, ],
      validation_frame = h2odigits.train[i, ],
      activation = "Tanh",
      autoencoder = TRUE,
      hidden = v$hidden,
      epochs = 30,
      sparsity_beta = 0,
      input_dropout_ratio = v$input_dr,
      hidden_dropout_ratios = v$hidden_dr,
      l1 = 0,
      l2 = 0
    )
  })
})

fm.res <- lapply(fm, function(m) {
  sapply(m, h2o.mse, valid = TRUE)
})

fm.res <- data.table(
  Model = rep(paste0("M", 1:6), each = 5),
  MSE = unlist(fm.res))

head(fm.res)

p.erate <- ggplot(fm.res, aes(Model, MSE)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", colour = "red") +
  theme_classic()
print(p.erate)

png("../FirstDraft/chapter04_images/B4228_04_09.png",
    width = 5.5, height = 5.5, units = "in", res = 600)
print(p.erate)
dev.off()


fm.res[, .(Mean_MSE = mean(MSE)), by = Model][order(Mean_MSE)]

fm.final <- h2o.deeplearning(
  x = xnames,
  training_frame = h2odigits.train,
  validation_frame = h2odigits.test,
  activation = "Tanh",
  autoencoder = TRUE,
  hidden = hyperparams[[4]]$hidden,
  epochs = 30,
  sparsity_beta = 0,
  input_dropout_ratio = hyperparams[[4]]$input_dr,
  hidden_dropout_ratios = hyperparams[[4]]$hidden_dr,
  l1 = 0,
  l2 = 0
)

fm.final
