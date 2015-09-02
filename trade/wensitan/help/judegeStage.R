
judegeStage = function(smaData,ln=10,sn=3,lpratio=4,spratio=2,lnratio=-4,snratio=-2)
{
 
  deltratio = na.omit(Delt(smaData) * 100)
  deltratioSlideLong = na.omit(runSum(deltratio,n=ln))
  
  deltratioSlideShort = na.omit(runSum(deltratio,n=sn))
  
  deltratioSlide = na.omit(merge(deltratioSlideLong,deltratioSlideShort))
  alldata = na.omit(merge(smaData,deltratioSlide))
  
  
  stage=apply(alldata,MARGIN=1,FUN=function(x){
    if(is.na(x['deltratioSlideLong']) || is.na(x['deltratioSlideShort']))
      return(-1)
    else
    {
      if(x['deltratioSlideLong']<= lnratio && x['deltratioSlideShort'] <= snratio) { return(4) }
      else if(x['deltratioSlideLong']>= lpratio && x['deltratioSlideShort'] >= spratio) {return(2)} 
      else {return(0)}
    }
    
  })
  names(stage) = ''
#   for(i in 2:length(stage))
#   {
#     if(stage[i]  == 0)
#     {
#       if(stage[i-1] == 2)
#         stage[i] = 3
#       else if(stage[i-1] == 4)
#         stage[i] = 1
#       else
#         stage[i] =  stage[i-1]
#     }
#   }
  stage = xts(stage,index(alldata))
  return(stage)
}