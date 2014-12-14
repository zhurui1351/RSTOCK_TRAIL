
#判读两条规则是否可以形成条件概率
#对于两条rule 前序相同，P(A)后一位为0,P(B)后一位不为0，再后面的位数都为0，可以计算条件概率
#对于c(1,0,0,0,0) 和 c(1,1,0,0,0) 可以计算出 周一涨后周二也涨的条件概率
isConditionProb <- function(r,r1)
{
  match = (r == r1)
  #P(A) P(B) 只会有一项规则不同
  if(length(match[match==FALSE]) != 1)
    return(FALSE)
  index = which(match==FALSE)
  #为0时才能计算条件概率
  if(r[index] !=0 && r1[index] !=0)
  {
    return(FALSE)
  }
  #如果不是最后一位，那么index后面的值都为0
  if(index != length(r))
  {
    if(!all(r[(index+1) : length(r)] == 0))
      return(FALSE)
    if(!all(r1[(index+1) : length(r1)] == 0))
      return(FALSE)
  }
  return(TRUE)
}
