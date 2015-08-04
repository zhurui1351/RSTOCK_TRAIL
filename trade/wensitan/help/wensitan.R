require("quantmod")

filterBasicOneDay = function(mg,daydate,indexp)
{
  daydate = '2012-07-13'
  allcodes = names(mg)
  currentindexp =indexp[daydate] 
  l = lapply(allcodes,function(p,date){
    n = mg[[p]]
    current = n[date]
    if(currentindexp$stage != 4 && nrow(current) == 1  && !is.na(current$stage) && current$stage == 1 && current$Close > current$sma30)
    {
      
      i = which(index(n) == date)
      if(i < 6 ) return(NULL)
      allZero = all(n[(i-5):i]$stage==0)
      excess =  (current$Close - current$sma30)/current$sma30
      if(!is.na(allZero) && allZero && excess>= 0.01)
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
  current = n[daydate]
  if(currentindexp$stage != 4 && nrow(current) == 1  && !is.na(current$stage) && current$stage == 1 && current$Close > current$sma30)
  {
    
    i = which(index(n) == daydate)
    if(i < 6 ) return(NULL)
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
x = x[,c('Close','sma30','mvratio','rsratio','stage')]
