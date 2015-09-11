growRatioGreaterThanDegree = function(daydate,mg,ratio=0.08)
{
  allcodes = names(mg)
  l = lapply(allcodes,function(p,date){
    # print(p)
    n = mg[[p]]
    current = n[date]
    if(nrow(current)==1 && !is.na(current$volatile) && current$volatile > 0)
    {
      print(p)
      return(p)
    }
    else
    {
      return(NULL)
    }
    
  }
  ,daydate)
}