#最大回撤算法，参照wiki百科
performanceInfo<-function(trades)
{
  result = list()
  trades = xts(as.numeric(trades),order.by=index(trades))
  #累积盈利
  tradeSum = cumsum(trades)
  peak = -1e+10
  peaktime = NULL
  temppeaktime = NULL
  maxDrawDownTime=NULL
  #最大回撤
  mdd = 0
  dd = rep(0,nrow(tradeSum)) 
  for(i in 1:nrow(tradeSum))
  {
    if(as.numeric(tradeSum[i]) > peak)
    {
       peak = as.numeric(tradeSum[i])
       temppeaktime = index(tradeSum[i])
    }
    dd[i] = peak - as.numeric(tradeSum[i])
    if(dd[i] > mdd)
    {
      peaktime = temppeaktime
      mdd = dd[i]
      maxDrawDownTime = index(tradeSum[i])
    }
  }
 # result$dd = dd
  result$mdd = mdd
  result$maxDrawDownFrom = peaktime
  result$maxDrawDownTo = maxDrawDownTime
  #最大连续亏损笔数
  totalLoss = 0
  maxLossNum = 0
  pre = FALSE
  numCons = 0
  losses = 0
  consecLossFrom = NULL
  consecLossTo = NULL
  lossFrom = NULL
  lossTo = NULL
  for(i in 1:nrow(trades))
  {
    rtn = as.numeric(trades[i])
    if(rtn <= 0)
    {
      if(pre == FALSE)
      {
        lossFrom = index(trades[i])
      }
      numCons = numCons + 1
      losses = losses + rtn
      pre = TRUE
    }
    else if (rtn >0 && pre ==TRUE)
    {
     
      lossTo = index(trades[i-1])
      pre = FALSE
      if(numCons > maxLossNum)
      {
        consecLossFrom = lossFrom
        consecLossTo = lossTo
        maxLossNum = numCons
        totalLoss = losses
        
      }
      losses = 0
      numCons = 0
    }
  }
  if(numCons > maxLossNum)
  {
    lossTo = index(trades[i-1])
    consecLossFrom = lossFrom
    consecLossTo = lossTo
    maxLossNum = numCons
    totalLoss = losses
  }
  result$maxLossNum = maxLossNum
  result$totalLossDuringMaxLossPeriod = totalLoss
  result$consecLossFrom = consecLossFrom
  result$consecLossTo = consecLossTo
  
  result$positiveTrades = nrow(trades[as.numeric(trades) > 0])
  result$negtiveTrades = nrow(trades[as.numeric(trades) < 0])
  
  result$grossProfit = sum(as.numeric(trades[as.numeric(trades) > 0]))
  result$grossLoss = sum(as.numeric(trades[as.numeric(trades) < 0]))
  
  result$percenPositive = result$positiveTrades / nrow(trades)
  result$netProfit = result$grossProfit + result$grossLoss
  
  result$totalTrades = nrow(trades)
  
  return(result)
}