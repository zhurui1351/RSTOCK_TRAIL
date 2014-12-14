#对基础概率以及条件概率进行过滤，使用prop.test进行测试
#大于一定概率 ，p-value在0.05以上的规则被认为是有显著统计特性的规则
#假设检验，原假设为比率大于confProp
getInterestingRules <- function(confProp,conf,base_prob,condition_rules)
{
  interestingRules = list()
  rule_index = 1
  #寻找基本概率满足条件的rule
  for(i in 1:length(base_prob))
  {
    #符合比例的个数
    satisfied = base_prob[[i]]$num_satisfied
    #总周数
    numweek = base_prob[[i]]$numweek
    if(numweek == 0)
      next
    #比例
    ratio = base_prob[[i]]$ratio
    #进行假设检验
    p = prop.test(satisfied,n=numweek,p=confProp,alternative =c("less"))
    #如果p-value 满足大于conf，则认为有显著性
    if(p$p.value > conf && ratio >=confProp)
    {
      rule = base_prob[[i]]
      rule$type = 'base'
      rule$pvalue = p$p.value
      interestingRules[[rule_index]] = rule
      rule_index = rule_index + 1
    }
  }
  if(length(condition_rules) == 0)
    return(interestingRules)
  #寻找条件概率满足条件的rule
  for(i in 1:length(condition_rules))
  {
    #符合比例的个数
    satisfied = condition_rules[[i]]$numcons
    #总周数
    numweek = condition_rules[[i]]$numpre
    if(numweek == 0)
      next
    #比例
    ratio = condition_rules[[i]]$prob
    #进行假设检验
    p = prop.test(satisfied,n=numweek,p=confProp,alternative =c("less"))
    #如果p-value 满足大于conf，则认为有显著性
    if(p$p.value > conf  && ratio >=confProp)
    {
      rule = condition_rules[[i]]
      rule$type = 'condition'
      rule$pvalue = p$p.value
      interestingRules[[rule_index]] = rule
      rule_index = rule_index + 1
    }
  }
  return(interestingRules)
}

