#根据rules集合，求所有的基础概率

getBasePropByRules <- function(rules,mydata,tradeDays=5)
{
  base_prob = list()
  #遍历所有的rule进行计算
  for(i in 1 : length(rules))
  {
    result = compareToRule(rules[[i]],mydata,tradeDays)
    base_prob[[i]] = list(rule=rules[[i]],ratio=result[1],num_satisfied=result[2],numweek=result[3])
  }
  return(base_prob)  
}