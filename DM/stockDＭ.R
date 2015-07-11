library(DMwR)
library(xts)
library(quantmod)
data(GSPC)
x1 = xts(rnorm(100),seq(as.POSIXct('2000-01-01'),len=100,by='day'))
x2 = xts(rnorm(100),seq(as.POSIXct('2000-01-01 13:00'),len=100,by='min'))
x3 =xts(rnorm(3),as.Date(c('2005-01-01','2005-01-02','2005-01-03')))

#多元时间序列

mts.vals = matrix(round(rnorm(25),2),5,5)
colnames(mts.vals) = paste('ts',1:5,sep='')
mts = xts(mts.vals,as.POSIXct(c('2003-01-01','2003-01-04','2003-01-05','2003-01-06','2003-02-16')))

index(mts)
coredata(mts)

T.ind = function(quotes,tgt.margin=0.025,n.days=10)
{
  v = apply(HLC(quotes),1,mean)
  r = matrix(NA,ncol=n.days,nrow = NROW(quotes))
  for(x in 1:n.days) r[,x] = Next(Delt(v,k=x),x)
  
  x = apply(r,1,function(x) sum(x[x>tgt.margin | x < -tgt.margin]))
  if(is.xts(quotes))
    xts(x,time(quotes))
  else
    x
}

#绘图
candleChart(last(GSPC,'3 months'),theme='white',TA=NULL)
avgPrice = function(p) apply(HLC(p),1,mean)
addAvgPrice = newTA(FUN=avgPrice,col=1,legend='AvgPrice')
add.ind = newTA(FUN=T.ind,col='red',legend='tgtRet')
addAvgPrice(on = 1)
add.ind()

#指标集

myATR = function(x) ATR(HLC(x))[,'atr']
mySMI = function(x) SMI(HLC(x))[,'SMI']
myADX = function(x) ADX(HLC(x))[,'ADX']
myAroon = function(x) aroon(x[,c('High','Low')])$oscillator
myBB = function(x) BBands(HLC(x))[,'pctB']
myChainkinVol = function(x) Delt(chaikinVolatility(x[,c('High','Low')]))[,1]
myCLV = function(x) EMA(CLV(HLC(x)))[,1]
myEMV = function(x) EMV(x[,c('High','Low')],x[,'Volume'])[,2]
myMACD = function(x) MACD(Cl(x))[,2]
myMFI = function(x) MFI(x[,c('High','Low','Close')],x[,'Volume'])
mySAR = function(x) SAR(x[,c('High','Close')])[,1]
myVolat = function(x) volatility(OHLC(x),calc='garman')[,1]

#构建随机森林模型
data.model = specifyModel(T.ind(GSPC)~Delt(Cl(GSPC),k=1:10) + myATR(GSPC)
                          + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + myBB(GSPC)
                          + myChainkinVol(GSPC) + myCLV(GSPC) + CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + myMACD(GSPC)
                          + myMFI(GSPC) + RSI(Cl(GSPC)) + mySAR(GSPC) + myVolat(GSPC) +runMean(Cl(GSPC))
                          +runSD(Cl(GSPC))
                          )

set.seed(1234)
#importance=T 会让随机森林估计变量的重要性
rf = buildModel(data.model,method='randomForest',training.per = c(start(GSPC),index(GSPC['1999-12-31'])),ntree=50,importance=T)
#检查变量重要性，第一类得分，也就是减少变量后带来的误差增长
varImpPlot(rf@fitted.model ,type=1)

imp = importance(rf@fitted.model,type=1)
#筛选出重要性大于10的变量
rownames(imp)[which(imp>10)]
#根据结果重新建立模型

data.model = specifyModel(T.ind(GSPC)~ myATR(GSPC)
                          + mySMI(GSPC) + myADX(GSPC) + myEMV(GSPC) + myMACD(GSPC)
                          +  mySAR(GSPC) + myVolat(GSPC) +runMean(Cl(GSPC))
                          +runSD(Cl(GSPC))
)
#构造模型的数据结构

Tdata.train = as.data.frame(modelData(data.model,data.window = c('1970-01-02','1999-12-31')))
Tdata.eval = na.omit(as.data.frame(modelData(data.model,data.window = c('2000-01-01','2009-09-15'))))
Tform = as.formula(' T.ind.GSPC ~ .')

#建模 
#神经网络 
set.seed(1234)
library(nnet)
#神经网络对标度敏感，因此先标准化标度,预测完后调用unscale恢复
norm.data = scale(Tdata.train)
nn = nnet(Tform,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,linout=T,trace=F)
norm.preds = predict(nn,norm.data[1001:2000,])
preds = unscale(norm.preds,norm.data)
