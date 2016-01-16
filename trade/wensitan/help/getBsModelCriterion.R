require(pROC)
require(ROCR)
getBsModelCriterion = function(longtotest,f,testdate,pricedata,analysedata)
{
  signals = data.frame()
  for(y in testdate)
  {
    #print(y)
    starttraindate = as.character(as.numeric(y) - longtotest)
    endtraindate = as.character(as.numeric(y) - 1)
    analysedata_train = as.data.frame(analysedata[paste(starttraindate,endtraindate,sep='/')])
    #最后一期的数据不参与建模，比如20141231那天是无法知道2015第一个交易日的涨跌
    analysedata_train = analysedata_train[1:(nrow(analysedata_train) - 1),]
    
    starttrain = head(rownames(analysedata_train),1)
    endtrain =  tail(rownames(analysedata_train),1)
    trainperiod = paste(starttrain,endtrain,sep='/')
    
    #根据测试期间的参数 更新整个数据 比如测试期间最优sma参数是 3 10 则用该参数重算sma信号
    #analysedata_update = getBsoptimsignalinvarset(pricedata,analysedata,starttrain,endtrain,start,end,comb)
    cachename = paste('analysedata',y,longtotest,sep='_')
    analysedata_update = get(cachename)
    
    if(is.null(analysedata_update))
      return(0)
    
    #用最优参数 计算训练期间的信号
    analysedata_train = analysedata_update[trainperiod]
    analysedata_train[analysedata_train == 'hold'] = NA
    
    #用最优参数计算 测试期间的信号
    analysedata_test = analysedata_update[y]
    analysedata_test[analysedata_test == 'hold'] = NA
    
    #建模
    
    model = naiveBayes(f,
                       data=as.data.frame(analysedata_train),na.action = na.pass)
    
    #预测
    pr = tryCatch(predict(model,as.data.frame(analysedata_test),type = 'raw'),
                  error = function(e){print(paste(paste0(f,collapse = ''),'error'));return(NULL)})
    if(is.null(nrow(pr))) return(0)
    
    #用新模型计算上一个周期最后一个交易日的预测，并更新到预测表
    # 比如2015年1月1日 更新模型后， 用新模型对2014-12-31的数据进行预测 来进行2015年第一个交易日的决策
    pf = predict(model,tail(analysedata_update[endtraindate],1),type='raw')
    
    
    #更新预测表，避免look ahead bias
    pr = rbind(pf,pr[1:(nrow(pr)-1),])
    
   # signal = ifelse(pr[,'down'] > 0.5,'down',ifelse(pr[,'up'] > 0.5,'up',NA))
   # signal = cbind(as.data.frame(analysedata_test[,'leadclflag']),signal=signal)
    signal = cbind(pr[,1],as.character(analysedata_test[,'leadclflag']))
    signals = rbind(signals,signal)
  }
  
  pred <- prediction(as.numeric(signals[,1]), signals[,2])
  perf <- performance(pred,"auc")
  print(perf@y.values[[1]])
  return(perf@y.values[[1]])
}

fitness = function(tb,p)
{
  pr = predict(tb, as.data.frame(p),type='raw')
  #tt = table(pr, p[,'leadclflag'])
  au = auc(as.character(p[,'leadclflag']),as.numeric(pr[,1]))
  #precise = (tt[1,1]+tt[2,2]) / sum(tt)
  return(as.numeric(au))
}