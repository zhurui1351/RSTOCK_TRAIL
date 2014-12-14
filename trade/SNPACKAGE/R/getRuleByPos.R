#根据位置参数设置一条rule，每个位置有涨（1）、跌（-1）两种选择
#比如输入参数c(1,2),返回c(1,1,0,0,0) c(1,-1,0,0,0) c(-1,1,0,0,0) c(-1,-1,0,0,0)
getRuleByPos<-function(posRule,tradeDays=5)
{
  if(length(posRule) == 1)
  {
    #只有一个位置，那么将该位置设为1或-1
    rule = rep(0,tradeDays)
    rule1 = rep(0,tradeDays)
    rule[posRule[1]] = 1
    rule1[posRule[1]] = -1
    return(list(rule,rule1))
  }
  else
  {
    #递归求解，先获得其他位置的rules，然后将对应位置设置为1或-1
    otherRule = getRuleByPos(posRule[2:length(posRule)],tradeDays)
    positiveRule = lapply(otherRule,function(x,n){x[n]=1;x},n=posRule[1])
    negRule = lapply(otherRule,function(x,n){x[n]=-1;x},n=posRule[1])
    #合并rule，形成行的rule列表
    rules = c(positiveRule,negRule)
    return(rules)
  }
}
