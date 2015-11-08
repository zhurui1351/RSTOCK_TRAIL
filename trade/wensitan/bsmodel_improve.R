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
getSymbols("^GSPC",from="1900-01-01")
pricedata = adjustOHLC(GSPC)
load('GSPC.Rdata')
colnames(pricedata) = gsub('GSPC.','',colnames(pricedata))
#pricedata = readSHindex()
pricedata = na.omit(pricedata)
#处理待预测数据，用lead提前一天，把头天的涨跌加入预测变量
cl = Cl(pricedata) - Op(pricedata)
clflag = ifelse(cl < 0 ,'down','up')
leadclflag = shift(clflag,n=1,type='lead')
analysedata = merge(leadclflag,clflag)

start = head(index(analysedata),1)
end = tail(index(analysedata),1)

testdate = substr(as.character(index(to.yearly(pricedata['1990/2015']))),1,4)
starttraindate = as.character(1995)
records = data.frame()
#c(4,6,11)
# 回测期间测试 4表示用前4年到前年的共计3年数据进行测试
for(longtotest in c(4,6,11,16))
{
  ##print(longtotest)
  for(y in testdate)
  {
    print(y)
    starttraindate = as.character(as.numeric(y) - longtotest)
    endtraindate = as.character(as.numeric(y) - 1)
    analysedata_train = as.data.frame(analysedata[paste(starttraindate,endtraindate,sep='/')])
    #最后一期的数据不参与建模，比如20141231那天是无法知道2015第一个交易日的涨跌
    analysedata_train = analysedata_train[1:(nrow(analysedata_train) - 1),]
    
    starttrain = head(rownames(analysedata_train),1)
    endtrain =  tail(rownames(analysedata_train),1)
    trainperiod = paste(starttrain,endtrain,sep='/')
    
    #根据测试期间的参数 更新整个数据 比如测试期间最优sma参数是 3 10 则用该参数重算sma信号
    analysedata_update = getBsoptimsignal(pricedata,analysedata,starttrain,endtrain,start,end)
    
    filename = paste('gspc',longtotest,sep='')
    filename = paste(filename,y,sep='')
    filename =  paste(filename,'Rdata',sep='.')
    save(analysedata_update,file=filename)
    #load(filename)
    
    #用最优参数 计算训练期间的信号
    analysedata_train = analysedata_update[trainperiod]
    analysedata_train[analysedata_train == 'hold'] = NA
    
    #用最优参数计算 测试期间的信号
    analysedata_test = analysedata_update[y]
    analysedata_test[analysedata_test == 'hold'] = NA
    
    
    #建模
    model = naiveBayes(leadclflag ~ . - Close ,data = as.data.frame(analysedata_train),na.action = na.pass)
    
    #预测
    pr = predict(model,as.data.frame(analysedata_test),type = 'raw')
    
    #用新模型计算上一个周期最后一个交易日的预测，并更新到预测表
    # 比如2015年1月1日 更新模型后， 用新模型对2014-12-31的数据进行预测 来进行2015年第一个交易日的决策
    pf = predict(model,tail(analysedata_update[endtraindate],1),type='raw')
    #更新预测表，避免look ahead bias
    pr = rbind(pf,pr[1:(nrow(pr)-1),])
    
    #生成回测记录
    for(i in 1:nrow(pr))
    {
      pv = pr[i,]
      tradetime = as.character(index(analysedata_test[i,]))
      
      numerindex = analysedata_update[tradetime][,3:ncol(analysedata_update)]
      #判断多少个信号有效
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
    #  save(records,file='bsrecords.Rdata')
  }
  
}