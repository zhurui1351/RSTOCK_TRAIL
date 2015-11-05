rm(list=ls(all=T))
require('quantmod')
require('TTR')
require('dygraphs')
require('lubridate')
require('dplyr')
require('data.table')
require('e1071')
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

#引入辅助函数
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/help')

#读入数据
pricedata = readSHindex()
pricedata = na.omit(pricedata)
#处理待预测数据，用lead提前一天，把头天的涨跌加入预测变量
cl = Cl(pricedata) - Op(pricedata)
clflag = ifelse(cl < 0 ,'down','up')
leadclflag = shift(clflag,n=1,type='lead')
analysedata = merge(leadclflag,clflag)
analysedata = na.omit(analysedata)

testdate = substr(as.character(index(to.yearly(pricedata['2000/2015']))),1,4)
starttraindate = as.character(1995)
records = data.frame()

for(y in testdate)
{
  print(y)
  endtraindate = as.character(as.numeric(y) - 1)
  #  starttrainyear = as.character(as.numeric(y) - 6)
  analysedata_train = as.data.frame(analysedata[paste(starttraindate,endtraindate,sep='/')])
  #最后一期的数据不参与建模，比如20141231那天是无法知道2015第一个交易日的涨跌
  analysedata_train = analysedata_train[1:(nrow(analysedata_train) - 1),]
  start = head(rownames(analysedata_train),1)
  end =  tail(rownames(analysedata_train),1)
  
  analysedata_test = as.data.frame(analysedata[y])
  analysedata_test = na.omit(analysedata_test)
  analysedata_test[analysedata_test == 'hold'] = NA
  
  analysedata_train = getBsoptimsignal(pricedata,analysedata,start,end)
  #建模
  model = naiveBayes(leadclflag ~ . - rsisignal - macdsignal - obvsignal,
                     data=analysedata_train,na.action = na.pass)
  #预测
  pr = predict(model,analysedata_test,type = 'raw')
  
  #用新模型计算上一个周期最后一个交易日的预测，并更新到预测表
  # 比如2015年1月1日 更新模型后， 用新模型对2014-12-31的数据进行预测 来进行2015年第一个交易日的决策
  pf = predict(model,tail(analysedata[endtraindate],1),type='raw')
  #更新预测表，避免look ahead bias
  pr = rbind(pf,pr[1:(nrow(pr)-1),])
  
  #生成回测记录
  for(i in 1:nrow(pr))
  {
    pv = pr[i,]
    tradetime = rownames(analysedata_test[i,])
    
    numerindex = analysedata[tradetime][,2:ncol(analysedata)]
    numerindex = sum(numerindex != 'hold')
    if(numerindex < 4) next;
    
    enter = as.numeric(Op(pricedata[tradetime]))
    out = as.numeric(Cl(pricedata[tradetime]))
    if(pv['down'] > 0.55)
    {    
      record = data.frame(code='index',opdate=tradetime,cldate=tradetime,Open=enter,Close=out,profit=as.numeric(out-enter),initStop=0,stopprice=0,type='short')
      records = rbind(records,record)
    }
    if(pv['up'] > 0.55)
    {
      record = data.frame(code='index',opdate=tradetime,cldate=tradetime,Open=enter,Close=out,profit=as.numeric(out-enter),initStop=0,stopprice=0,type='long')
      records = rbind(records,record)
    }
  }
}

