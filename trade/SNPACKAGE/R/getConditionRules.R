#根据基础概率 获得所有的条件概率
getConditionRules <- function(mybase_prob)
{
  condition_rules <- list()
  index_condition_rule <- 1
  #对基础概率两两匹配
  for(i in 1 : length(mybase_prob))
  {
    
    rule = mybase_prob[[i]]
    j = i + 1
    while(j<=length(mybase_prob))
    {
      rule1 = mybase_prob[[j]]
      #如果两个规则可以合并
      if(isConditionProb(rule$rule,rule1$rule))
      {
        newRule = conditionProb(rule,rule1)
        #保存得到的条件概率
        condition_rules[[index_condition_rule]] = newRule
        index_condition_rule = index_condition_rule + 1
      }
      j = j + 1
    }
    
  }
  
  return(condition_rules)
}
