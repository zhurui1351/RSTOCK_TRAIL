#依照一条规则进行模式匹配，遍历数据，按交易日划分，检查是否匹配规则
compareToRule <- function(rule,mydata,tradeDays=5)
{
  total = nrow(mydata)
  i = 1
  #数据中总共含有多少周
  numweek = 0
  #符合规则的周数
  num_satisfied = 0
  while(i <= total)
  {
    #周内循环
    infoInOneWeek = getVolatility(mydata,i,tradeDays)
    vol = infoInOneWeek[[1]]
    num = infoInOneWeek[[2]]
    i = i + num
    #如果符合规则，那么结果加1
    if(isSatisfiedRule(data=as.integer(vol),rule=rule))
    {
      num_satisfied = num_satisfied + 1    
    }
    numweek = numweek + 1
  }
  ratio = num_satisfied / numweek
  return(c(ratio,num_satisfied,numweek))
}
