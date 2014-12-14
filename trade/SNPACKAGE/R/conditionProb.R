#计算条件概率
conditionProb <- function(baseProb1,baseProb2)
{
  #检验哪个条件作为前验条件
  if(baseProb1$num_satisfied <= baseProb2$num_satisfied)
  {
    prob = baseProb1$num_satisfied / baseProb2$num_satisfied
    rule = list(pre=baseProb2$rule,cons=baseProb1$rule,prob=prob,numpre=baseProb2$num_satisfied,numcons=baseProb1$num_satisfied)
  }
  else
  {
    prob = baseProb2$num_satisfied / baseProb1$num_satisfied
    rule = list(pre=baseProb1$rule,cons=baseProb2$rule,prob=prob,numpre=baseProb1$num_satisfied,numcons=baseProb2$num_satisfied) 
  }
  return(rule)
}
