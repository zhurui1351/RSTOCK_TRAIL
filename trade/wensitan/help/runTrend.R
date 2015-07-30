require(compiler)
runTrend = cmpfun(function(data,n=10)
{
  
  getratio = function(y)
  {
    x = 1:length(y)
    f = lm(y~x-1)
    coff = f$coefficients[[1]]
    return(coff)
  }
  
 # deltratio = na.omit(Delt(data) * 100)
  #deltratioSlide = na.omit(runSum(deltratio,n=n))
  speedratio = rollapply(data,width=n,FUN=getratio)
  return(na.omit(speedratio))
})