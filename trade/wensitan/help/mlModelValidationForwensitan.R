mlModelValidationForwensitan = function(recordsinfo)
{ 
  profitframe = data.frame()
  for( i in 2003:2014)
  {
    print(i)
    testyear = 1996:i
    trainyear = c(i+1)
    testset = recordsinfo[which(as.numeric(substr(recordsinfo$opdate,1,4)) %in% testyear),]
    trainset = recordsinfo[which(as.numeric(substr(recordsinfo$opdate,1,4)) %in% trainyear),]
    model <- nnet(profitflags ~ stage + votile  + preatr + prevolatile + presma30
                          +premeanvo+premvratio+prers,data = testset,size=2)
    model <- glm(profitflags ~ Open + stage +  prevolatile + presma30
                 +premeanvo+prers,data = testset,family = 'binomial')
    profitpredict = predict(model,subset(trainset,select=c(Open, stage,votile,preatr,prevolatile,presma30
                                                            ,premeanvo,premvratio,prers)),type='class')
    profitpredict = ifelse(profitpredict>0.2,'good','bad')
    dt = table(trainset[,'profitflags'],profitpredict)
    predicttrade = which(profitpredict == 'good')
    predicttrade = trainset[predicttrade,]
    profit = as.numeric(predicttrade[,'profit'])
    totaltrades = length(profit)
    totalprofit = sum(profit)
    minprofit = min(profit)
    maxprofit = max(profit)
    winratio = length(profit[profit>0]) / length(profit)
    profitframe = rbind(profitframe,data.frame(year=(i+1),totaltrades=totaltrades,totalprofit=totalprofit,winratio=winratio,
                                 minprofit=minprofit,maxprofit=maxprofit))
  }
  print(profitframe)
}