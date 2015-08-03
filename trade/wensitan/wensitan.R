require("quantmod")

filterBasicOneDay = function(mg,daydate,indexp)
{
  daydate = '2015-07-31'
  allcodes = names(mg)
  currentindexp =indexp[daydate] 
  l = lapply(allcodes,function(p,date){
    n = mg[[p]]
    current = n[date]
    if(currentindexp$stage != 4 && nrow(current) == 1  && !is.na(current$stage) && current$stage == 0 && current$Close > current$sma30)
    {
      
      i = which(index(n) == date)
      if(i < 3 ) return(NULL)
      allZero = all(n[(i-5):i]$stage==0)
      if(!is.na(allZero) && allZero)
      {
        return(p)
      }
      else
      {
        return(NULL)
      }
       
    }
    else
    {
      return(NULL)
    }
      
  }
  ,daydate)
  
  l = Filter(function(x){!is.null(x)},l)
}

for(p in allcodes)
{
  n = mg[[p]]
  current = n[date]
  if(currentindexp$stage != 4 && nrow(current) == 1  && !is.na(current$stage) && current$stage == 0)
  {
    
    i = which(index(n) == date)
    if(i < 3 ) return(NULL)
    allZero = all(n[(i-5):i]$stage==0)
    if(allZero)
    {
      print(1)
    }
    else
    {
      print(0)
    }
    
  }
  else
  {
    print(0)
  }
}
