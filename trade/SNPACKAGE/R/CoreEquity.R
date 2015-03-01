CoreEquity <- function(ratio)
{
  CoreEquityCompute = function(...){
   para = list(...)
   if(is.null(para$price) || is.null(para$eq))
    {
      print(para)
      stop('error parameters in CoreEquity')
    }
    return(as.numeric(trunc(para$eq * ratio/para$price)))
  }
  return(CoreEquityCompute)
}