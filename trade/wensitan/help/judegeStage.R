
judegeStage = function(smaData)
{
  getratio = function(y)
  {
    x = 1:length(y)
    f = lm(x~y)
    coff = f$coefficients[[2]]
    return(coff)
  }
  deltratio = na.omit(Delt(smaData) * 100)
  deltratioSlideLong = na.omit(runSum(deltratio,n=10))
  deltratioSlideShort = na.omit(runSum(deltratio,n=3))
  deltratioSlide = na.omit(merge(deltratioSlideLong,deltratioSlideShort))
  alldata = na.omit(merge(smaData,deltratioSlide))
  
  
  stage=apply(alldata,MARGIN=1,FUN=function(x){
    if(is.na(x['deltratioSlideLong']) || is.na(x['deltratioSlideShort']))
      return(-1)
    else
    {
      if(x['deltratioSlideLong']<= -2 && x['deltratioSlideShort'] <= -2) { return(4) }
      else if(x['deltratioSlideLong']>= 2 && x['deltratioSlideShort'] >= 2) {return(2)} 
      else {return(0)}
    }
    
  })
  names(stage) = ''
  stage = xts(stage,index(alldata))
  return(stage)
}