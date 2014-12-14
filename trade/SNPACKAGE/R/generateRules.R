#rules表示规则 1为涨，-1为跌，0表示不考虑，位置索引值代表周几
#rules <- c(-1,1,0,0,0) 表示周一跌，周二涨的情况
#产生所有的规则，对于每个关心的天，可以设置为关心涨（1）、跌（-1）、其余为0 表示不关心
#只对rules小于等于prune的rule感兴趣
generateRules<-function(prune=5,tradeDays=5)
{
  rules = list()
  #产生所有的位置组合
  pos = generateRulespos(c(1:tradeDays))
  for(i in 1:length(pos))
  {
    #剪枝
    if(length(pos[[i]]) > prune)
    {
      next
    }
    #对关心的位置进行赋值
    rule = getRuleByPos(pos[[i]],tradeDays)
    rules = c(rule,rules)
  }
  return(rules)
}
