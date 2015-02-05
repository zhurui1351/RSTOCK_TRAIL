SnReport <- function(priceData,confProp=0.55,conf=0.05,tradeDays=5,prune=5,profitratio=0.2,drawdownratio=-0.1,initEq = 100000,path='',type='iw')
{
  snrules = SNRules(priceData,confProp=confProp,conf=conf,tradeDays=tradeDays,prune=prune,type=type)
  traderules = getTradeRules(snrules)
  reports = list(NULL)
  reportIndex = 1
  if(length(traderules)  == 0)
    return(list(NULL))
  for(i in 1:length(traderules))
  {
    traderule = traderules[[i]]$traderule
    print("now testing:  ")
    print(traderule)
    print(type)
    report = backTestBySnRule(traderule$rule,priceData,tradeDays=tradeDays,traderule$buyday,traderule$sellday,short = traderule$short,verbose=FALSE,initEq=initEq,type=type)
    perfomance = report$totalStates
    pratio = perfomance$End.Equity / initEq
    ddownRatio = perfomance$Max.Drawdown / initEq
    print(path)
    print(i)
    print("rule:")
    print(traderule)
    print("profit ratio:")
    print(pratio)
    print("max drawndown ratio:")
    print(ddownRatio)
    #不满足条件
    if(pratio < profitratio || ddownRatio < drawdownratio)
    {
      next
    }
    else
    {
      reports[[reportIndex]] = list(snrule=snrules[[i]],perfomance = perfomance,txns=report$txns)
      reportIndex = reportIndex + 1
      #生成测试报告
      if(path != '')
      {
        #名称要和backTestBySnRule里面的一样，用于绘制资金曲线
        strategy = "mydata";
        subdir= paste(path,"/",i,sep="")
        #生成报告目录以及子目录
        if(!file.exists(path))
        {
          dir.create(path,recursive=T)
        }
        if(!file.exists(subdir))
        {
          dir.create(subdir,recursive=T)
        }
        picname = paste(subdir,"/equitcurve.jpg",sep="") 
        jpeg(picname)
        chart.Posn(Portfolio=strategy,Symbol=strategy)
        dev.off()
        #交易明细
        txnsName = paste(subdir,"/txns.csv",sep="")
        write.zoo(report$txns,txnsName)
        #每笔交易
        pername = paste(subdir,"/per.csv",sep="")
        write.csv(report$perState,pername)
        #汇总
        sumname = paste(subdir,"/summary.txt",sep="")
        write("conditon: ",sumname)
        write(traderule$rule,sumname,append=T)
        write("buyday: ",sumname,append=T)
        write(traderule$buyday,sumname,append=T)
        write("sellday: ",sumname,append=T)
        write(traderule$sellday,sumname,append=T)
        write("short: ",sumname,append=T)
        write(traderule$short,sumname,append=T)
        write("profit ratio: ",sumname,append=T)
        write(pratio,sumname,append=T)
        write("max drawdown ratio: ",sumname,append=T)
        write(ddownRatio,sumname,append=T)
        write("percent positive: ",sumname,append=T)
        write(perfomance$Percent.Positive,sumname,append=T)
        
        write.csv(perfomance,paste(subdir,"/performance.csv",sep=""))
        save(.blotter,file = paste(subdir,"/blotter",sep=""))
      }
    }
  }
  return(reports)
}