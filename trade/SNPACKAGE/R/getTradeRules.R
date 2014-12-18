#输入sn算法返回的规则列表，产生交易规则
#对于基础规则，使用最后有效日为交易日，有效日置为0 为前提规则
#对于条件概率，前提规则为返回的前置规则，交易为后置规则
#交易日当天涨跌为正的话，做多 为负的话 做空

getTradeRules <- function(intrestingRules)
{
  tradeRules = list()
  tradeRuleIndex = 1
  if(length(intrestingRules) == 0)
    return(list())
  for( i in 1 : length(intrestingRules))
  {
    rule = intrestingRules[[i]]
    #基础概率规则
    if(rule$type == 'base')
    {
      tradeRule = getBaseTradeRule(rule$rule)
      tradeRules[[tradeRuleIndex]] = list(probrule=rule,traderule=tradeRule)
      tradeRuleIndex = tradeRuleIndex + 1
    }
    #条件概率
    if(rule$type == 'condition')
    {
      tradeRule = getConditionTradeRule(rule$pre,rule$cons)
      tradeRules[[tradeRuleIndex]] = list(probrule=rule,traderule=tradeRule)
      tradeRuleIndex = tradeRuleIndex + 1
    }
  }
  return(tradeRules)
}
