mlModelValidationForwensitan = function(recordsinfo)
{ 
  for( i in 2003:2014)
  {
    print(i)
    testyear = 1996:i
    trainyear = c(i+1)
    testset = recordsinfo[which(as.numeric(substr(recordsinfo$opdate,1,4)) %in% testyear),]
    trainset = recordsinfo[which(as.numeric(substr(recordsinfo$opdate,1,4)) %in% trainyear),]
    model <- svm(profitflags ~ stage + votile  + initStop + preclose + prevolatile,data = testset)
    profitpredict = predict(model,subset(trainset,select=c(prevolatile,preclose,stage,votile,initStop)),type='response')
    dt = table(trainset[,'profitflags'],profitpredict)
    predicttrade = which(profitpredict == 'good')
    predicttrade = trainset[predicttrade,]
    profit = as.numeric(predicttrade[,'profit'])
    totaltrades = length(profit)
    totalprofit = sum(profit)
    minprofit = min(profit)
    maxprofit = max(profit)
    winratio = length(profit[profit>0]) / length(profit)
    print(data.frame(year=(i+1),totaltrades=totaltrades,totalprofit=totalprofit,winratio=winratio,
                        minprofit=minprofit,maxprofit=maxprofit))
  }
}