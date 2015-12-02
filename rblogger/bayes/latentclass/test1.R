#http://www.r-bloggers.com/example-8-21-latent-class-analysis/
require('poLCA')
ds = read.csv("http://www.math.smith.edu/r/data/help.csv")
ds = within(ds, (cesdcut = ifelse(cesd>20, 1, 0)))
res2 = poLCA(cbind(homeless=homeless+1, 
                   cesdcut=cesdcut+1, satreat=satreat+1, 
                   linkstatus=linkstatus+1) ~ 1, 
             maxiter=50000, nclass=3, 
             nrep=10, data=ds)

#http://www.r-bloggers.com/latent-class-modeling-election-data/
set.seed(1234)

library(e1071)
library(poLCA)
library(reshape2)

## An example of simulating likert scale data
probs = cbind(c(.4,.2/3,.2/3,.2/3,.4),c(.1/4,.1/4,.9,.1/4,.1/4),c(.2,.2,.2,.2,.2))
my.n = 1000
my.len = ncol(probs)*my.n
raw = matrix(NA,nrow=my.len,ncol=3)
raw = NULL
for(i in 1:ncol(probs)){
  raw = rbind(raw, cbind(i,rdiscrete(my.n,probs=probs[,i],values=1:5)))
}
raw = data.frame(id = seq(1,my.n),raw)

# An example of how to transform data back from normalized data to a flat file
raw.flat = dcast(raw, id ~ i, value.var="V2")
names(raw.flat) = c("id","A","B","C")

# Simulation example of latent class models
f = cbind(B, C) ~ A
lca.fit1 <- poLCA(f,raw.flat,nclass=1, nrep=5)
lca.fit2 <- poLCA(f,raw.flat,nclass=2, nrep=5)

f = cbind(A, B, C)~1
lca.fit1 <- poLCA(f,raw.flat,nclass=1, nrep=5)
lca.fit2 <- poLCA(f,raw.flat,nclass=2, nrep=5)
# Example dataset from the poLCA package
data(election)
# build the model with PARTY as the covariate
f <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
            MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~PARTY

# Run LCA on the ANES 2000 dataset 3 classes
anes2000 <- poLCA(f,election,nclass=3,nrep=5)

# Build a matrix to prepare for graphing
my.mat.max = 15
my.mat <- cbind(1,c(1:my.mat.max))
exb <- exp(my.mat %*% anes2000$coeff)

# Run the matrix plot
matplot(c(1:my.mat.max),(cbind(1,exb)/(1+rowSums(exb))),ylim=c(0,1),type="l",
        main="Party ID as a predictor of candidate affinity class",
        xlab="Party ID: strong Democratic (1) to strong Republican (7)",
        ylab="Probability of latent class membership",lwd=2,col=c('blue','green','red'))
text(5.9,0.35,"Other")
text(5.4,0.7,"Bush affinity")
text(2.5,0.6,"Gore affinity")


table.raw = rbind(
  cbind( rep('W', 1286), rep('18-29', 1286), rep('O', 1286) ),
  cbind( rep('W', 3395), rep('30-44', 3395), rep('O', 3395) ),
  cbind( rep('W', 5239), rep('45-64', 5239), rep('O', 5239) ),
  cbind( rep('W', 2417), rep('65+', 2417), rep('O', 2417) ),
  cbind( rep('B', 534), rep('18-29', 534), rep('O', 534) ),
  cbind( rep('B', 404), rep('30-44', 404), rep('O', 404) ),
  cbind( rep('B', 404), rep('45-64', 404), rep('O', 404) ),
  cbind( rep('B', 104), rep('65+', 104), rep('O', 104) ),
  cbind( rep('H', 967), rep('18-29', 967), rep('O', 967) ),
  cbind( rep('H', 749), rep('30-44', 749), rep('O', 749) ),
  cbind( rep('H', 741), rep('45-64', 741), rep('O', 741) ),
  cbind( rep('H', 247), rep('65+', 247), rep('O', 247) ),
  cbind( rep('O', 197), rep('18-29', 197), rep('O', 197) ),
  cbind( rep('O', 197), rep('30-44', 197), rep('O', 197) ),
  cbind( rep('O', 197), rep('45-64', 197), rep('O', 197) ),
  cbind( rep('O', 197), rep('65+', 197), rep('O', 197) ),
  
  cbind( rep('W', 1490), rep('18-29', 1490), rep('R', 1490) ),
  cbind( rep('W', 1339), rep('30-44', 1339), rep('R', 1339) ),
  cbind( rep('W', 2388), rep('45-64', 2388), rep('R', 2388) ),
  cbind( rep('W', 1302), rep('65+', 1302), rep('R', 1302) ),
  cbind( rep('B', 247), rep('18-29', 247), rep('R', 247) ),
  cbind( rep('B', 627), rep('30-44', 627), rep('R', 627) ),
  cbind( rep('B', 648), rep('45-64', 648), rep('R', 648) ),
  cbind( rep('B', 162), rep('65+', 162), rep('R', 162) ),
  cbind( rep('H', 85), rep('18-29', 85), rep('R', 85) ),
  cbind( rep('H', 40), rep('30-44', 40), rep('R', 40) ),
  cbind( rep('H', 56), rep('45-64', 56), rep('R', 56) ),
  cbind( rep('H', 16), rep('65+', 16), rep('R', 16) ),
  cbind( rep('O', 61), rep('18-29', 61), rep('R', 61) ),
  cbind( rep('O', 61), rep('30-44', 61), rep('R', 61) ),
  cbind( rep('O', 61), rep('45-64', 61), rep('R', 61) ),
  cbind( rep('O', 61), rep('65+', 61), rep('R', 61) )
)

exitpoll2012 = data.frame(table.raw)
names(exitpoll2012) = c("RACE","AGE","VOTE")
table(table.raw[,1], table.raw[,2])
table(table.raw[,1], table.raw[,3])
f <- cbind(AGE, RACE)~VOTE
xp.lca <- poLCA(f,exitpoll2012,nclass=2)
table(exitpoll2012$AGE)
# Build a matrix to prepare for graphing
my.mat.max = 4
my.mat <- cbind(1,c(1:my.mat.max))
exb <- exp(my.mat %*% xp.lca$coeff)

# Run the matrix plot
matplot(c(1:my.mat.max),(cbind(1,exb)/(1+rowSums(exb))),ylim=c(0,1),type="l",
        main="Candidate Vote as a Predictor of Candidate Affinity Class using Voter Race and Age",
        xlab="Candidate Vote: Obama (1) Romney (2)",
        ylab="Probability of latent class membership",lwd=2,col=c('blue','red'))
text(1.4,0.25,"Romney Leaning")
text(1.4,0.8,"Obama Leaning")


#http://blog.sina.com.cn/s/blog_6e354a5f0100om3f.html
library("flexmix")
data("NPreg")
summary(NPreg)
m1 <- flexmix(yn ~ x + I(x^2), data = NPreg, k = 2)
m1
parameters(m1, component = 1)
parameters(m1, component = 2)
table(NPreg$class, clusters(m1))
summary(m1)
rm1 <- refit(m1)
summary(rm1)
m2 <- flexmix(yp ~ x, data = NPreg, k = 2, model = FLXMRglm(family = "poisson"))
summary(m2)
m3 = flexmix(~x, data=NPreg, k=2, model=list(FLXMRglm(yn~.+I(x^2)), FLXMRglm(yp~., family="poisson")))
summary(m3)
m4 <- flexmix(yn ~ x + I(x^2) | id2, data = NPreg, k = 2)
summary(m4)