#根据条件概率规则，产生交易规则，前提规则为返回的前置规则，交易日为后置规则
#交易日当天涨跌为正的话，做多 为负的话 做空
getConditionTradeRule <- function(prerule,postrule)
{
  pos = (prerule == postrule)
  pos = which(pos == FALSE)
  #获得交易日当天涨跌
  volatity = postrule[pos]
  
  short = FALSE
  if(volatity < 0)
    short = TRUE
  
  traderule = list(rule = prerule,buyday=pos,sellday=pos,short=short)
  return(traderule)
}