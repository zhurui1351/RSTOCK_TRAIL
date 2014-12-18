#回测一条规则
backTestBySnRule <- function(rule,pdata,buyday,sellday,short=F,initEq = 100000,verbose=TRUE,tradeDays=5)
{
  #清除环境变量
 # rm("mydata",pos = .blotter)
 # if(exists(".blotter")) 
  #{
   # rm(.blotter)
  #}
  .blotter <<- new.env()
  #blotter包需要全局变量
  mydata <<- pdata
  #assign("mydata",pdata,pos = .GlobalEnv)
  stockdata = initialData(mydata)
  strategy <- "mydata"
  #初始化货币
  currency("USD")
  #初始化证券
  stock(strategy, currency = "USD", multiplier = 1)
  #设立时区
  Sys.setenv(TZ = "UTC") 
  #初始化组合和账户
  initPortf(strategy, strategy, initDate = "1950-01-01")
  initAcct(strategy, portfolios = strategy, initDate = "1950-01-01", initEq = initEq) #初始的资金是1，000，000
  
  #交易日
  tradeDays = getTradeInfoInByRule(rule,stockdata,buyday,sellday,tradeDays=tradeDays)
  #对交易日的数据逐个处理
  for( i in 1:length(tradeDays) )
  {
    currentData = tradeDays[[i]]$data
    currentType = tradeDays[[i]]$type
    #获取当前交易日期
    currentDate <- time(currentData)
    #获取最近的头寸
    equity<-getEndEq(strategy, currentDate)
    #当前仓位
    posn <- getPosQty(strategy, Symbol=strategy, Date=currentDate)
    #获取当前收盘价
    closePrice <- as.numeric(Cl(currentData))
    #获取当前开盘价
    openPrice <- as.numeric(Op(currentData))
    #全仓交易
     #unitSize <-as.numeric(trunc(equity/closePrice))
    unitSize <- 1000
    #type 为 buy 加仓， 为sell 平仓 
    # 如果short为true 则是卖空
    
    if(posn == 0 && currentType == 'buy')
    {
      #做空
      if(short == TRUE)
      {
        addTxn(Portfolio=strategy, Symbol=strategy,
               TxnDate=currentDate, TxnPrice=openPrice,
               TxnQty = -1 * unitSize , TxnFees=0, verbose=verbose)
      }#做多
      else
      {
        addTxn(Portfolio=strategy, Symbol=strategy,
               TxnDate=currentDate, TxnPrice=openPrice,
               TxnQty = unitSize , TxnFees=0, verbose=verbose)
      } 
    }
    if(posn > 0 && currentType == 'sell')
    {
      addTxn(Portfolio=strategy, Symbol=strategy, TxnDate=currentDate,
             TxnPrice=closePrice, TxnQty = -1 * posn , TxnFees=0, verbose=verbose)
    }
    if(posn < 0 && currentType == 'sell')
    {
      addTxn(Portfolio=strategy, Symbol=strategy, TxnDate=currentDate,
             TxnPrice=closePrice, TxnQty = -1 * posn , TxnFees=0, verbose=verbose)
    }
    
    #更新账户、头寸信息
    updatePortf(strategy,strategy,Dates = currentDate)
    updateAcct(strategy, Dates = currentDate)
    updateEndEq(strategy, Dates = currentDate)
    
  }
  #获取最终资金额
 #  pdf("c:/1.pdf")
  #绘制资金曲线图
  chart.Posn(Portfolio=strategy,Symbol=strategy)
  #获得总的交易信息，如笔数、胜负率等
  states<-tradeStats(strategy,strategy)
  #获取每笔交易的信息
  perstates<-perTradeStats(strategy,strategy)
 # dev.off()
  txns = getTxns(strategy,strategy)
  #print(perstates)
  return(list(totalStates=states,txns=txns,perState=perstates))
  
}


