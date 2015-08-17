require("quantmod")

filterBasicOneDay = function(daydate,mg,indexp)
{
  #daydate = '2013-01-18'
 # print(daydate)
  allcodes = names(mg)
  #print(allcodes)
  currentindexp =indexp[daydate] 
  l = lapply(allcodes,function(p,date){
    n = mg[[p]]
    current = n[date]
    if(currentindexp$stage != 4 && nrow(current) == 1  && !is.na(current$stage) && !is.na(current$rs) && !is.na(current$rsratio) && current$stage == 0 && current$Close > current$sma30)
    {
      
      i = which(index(n) == date)
      if(i < 6 ) return(NULL)
      allZero = all(n[(i-5):(i-1)]$stage==0)
      #first jump cross sma30
      allexcess = (n[(i-5):(i-1)]$Close - n[(i-5):(i-1)]$sma30)
      allexcess = all(allexcess <= 0 )
      
      #current bigger than 0.1
      excess =  (current$Close - current$sma30)/current$sma30
      if(!is.na(allZero) && allZero && allexcess && excess>= 0.01 &&  current$rs>= 0.5  && current$mvratio> 1.5)
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
 # l = list(l)
#  names(l) = daydate
  return(l)
}
# 
# for(p in allcodes)
# {
#   n = mg[[p]]
#   current = n[daydate]
#   if(currentindexp$stage != 4 && nrow(current) == 1  && !is.na(current$stage) && current$stage == 1 && current$Close > current$sma30)
#   {
#     
#     i = which(index(n) == daydate)
#     if(i < 6 ) return(NULL)
#     allZero = all(n[(i-5):i]$stage==0)
#     if(allZero)
#     {
#       print(1)
#     }
#     else
#     {
#       print(0)
#     }
#     
#   }
#   else
#   {
#     print(0)
#   }
# }
# x = x[,c('Open','Close','sma30','volatile','mvratio','rs','rsratio','stage','atr')]
