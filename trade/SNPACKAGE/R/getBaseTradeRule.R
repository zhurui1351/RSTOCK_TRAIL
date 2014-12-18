#使用基础概率规则产生交易规则
#使用最后有效日为交易日，有效日置为0 为前提规则 交易日当天涨跌为正的话，做多 为负的话 做空
getBaseTradeRule<-function(baserule)
{
  pos = which(baserule!=0)
  #取最后一个不为0的交易日
  pos = pos[length(pos)]
  #当天涨跌
  volatity = baserule[pos]
  #产生前提条件
  pre = baserule
  pre[pos] = 0
  short = FALSE
  if(volatity < 0)
    short = TRUE
  #日内交易
  tradeRule = list(rule=pre,buyday=pos,sellday=pos,short = short)
  return(tradeRule)
            
}