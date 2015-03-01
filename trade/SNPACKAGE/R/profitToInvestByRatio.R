#将利润的百分比投入头寸中,新加入的头寸算作本金
profitToInvestByRatio <- function(ratio=0.01,profitratio = 0.1)
{
  computePosition = function(...)
  {
    para = list(...)
    if( is.null(para$eq) || is.null(para$initeq))
    {
      print(para)
      stop('error parameters in profitToInvestByRatio')
    }
    #根据复利公式计算
    n =log(para$eq / para$initeq,(1+profitratio)) 
    #向下取整,用于投资的总资金
    en = floor(n)
    #亏损状态的话，使用总资金模型
    if(en <  1)
    {
      return(as.numeric(trunc(para$eq * ratio/para$price)))
    }
    else
    {
      return(as.numeric(trunc(para$eq * ratio * (1+profitratio)^en/para$price)))
    }
  }
  return(computePosition)
}