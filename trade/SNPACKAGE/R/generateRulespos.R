#该函数产生所有关心涨跌的位置指数比如对于c(1,2)产生c(1),c(2),c(1,2)三个组合
#代表关心周一涨跌或者周二涨跌，或者周一周二涨跌都关心
generateRulespos <- function(days){
  if(length(days) == 1)
  {
    return(list(days[1]))
  }
  else
  {
    #递归产生所有子集,产生不包括本元素的所有组合
    rules = generateRulespos(days[2:length(days)])
    #将本元素附加上去
    extend_rules = lapply(rules,function(x,n){c(n,x)},n=days[1])
    #最终的位置包括不含本元素的组合、只含本元素的组合、本元素和其他元素的组合列表
    rules = c(list(days[1]),rules,extend_rules)
    return(rules)
  }
}
