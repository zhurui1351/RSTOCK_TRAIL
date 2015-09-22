growRatioGreaterThanDegree = function(daydate,mg,ratio=0.08)
{
  allcodes = names(mg)
  l = lapply(allcodes,function(p,date){
   # print(p)
    n = mg[[p]]
    current = n[date]
    if(nrow(current)==1 && !is.na(current$volatile) && current$volatile > ratio)
    {
      return(p)
    }
    else
    {
      return(NULL)
    }
    
  }
  ,daydate)
}

growRatioGreaterThanDegreeWithIndex = function(daydate,mg,ratio=0.08,shindex)
{
  allcodes = names(mg)
  l = lapply(allcodes,function(p,date){
    # print(p)
    n = mg[[p]]
    current = n[date]
    currentindex = shindex[date]
    if(nrow(current)==1 && nrow(currentindex)==1 && !is.na(current$volatile) && current$volatile > ratio && !is.na(currentindex$volatile) && currentindex$volatile < 0 )
    {
      return(p)
    }
    else
    {
      return(NULL)
    }
    
  }
  ,daydate)
}