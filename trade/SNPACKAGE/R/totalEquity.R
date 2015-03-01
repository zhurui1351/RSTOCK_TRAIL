totalEquity <- function(ratio=0.1)
{
  totalEquityCompute = function(...){
    para = list(...)
    if(is.null(para$price) || is.null(para$initeq))
    {
      print(para)
      stop('error parameters in CoreEquity')
    }
    return(as.numeric(trunc(para$initeq * ratio/para$price)))
  }
  return(totalEquityCompute)
}