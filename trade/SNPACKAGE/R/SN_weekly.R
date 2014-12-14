#周内SN算法
#假设检验，原假设为比率大于confProp,pvalue大于conf，规则长度小于prune的规则 p-vaule 大于conf规则被认为是有显著统计特性的规则
SN_weekly <- function(stockdata,confProp=0.6,conf=0.05,tradeDays=5,prune=5)
{
  #初始化数据
  stockdata = initialData(stockdata)
  
  #过滤条件，只对长度小于prune的规则感兴趣
  
  #每条规则 1为涨，-1为跌，0表示不考虑，位置索引值代表周几 
  #比如周一涨，周二跌,其余不关心 表示为 ,c(1,-1,0,0,0)
  #产生所有的周内规则
  rules = generateRules(prune=prune,tradeDays)
  #保存基础的rule，以及出现的频数、比例
  base_prob = getBasePropByRules(rules,stockdata,tradeDays)
  
  #计算条件概率
  condition_rules = getConditionRules(base_prob)
  #得到有意思的条件集合
  interestingRules = getInterestingRules(confProp,conf,base_prob,condition_rules)
  
  return(interestingRules)
}
