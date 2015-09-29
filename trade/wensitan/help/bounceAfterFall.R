bounceAfterFall = function(daydate,mg,ratio=-0.08,nday=3,shindex=NULL)
{
  allcodes = names(mg)
  l = lapply(allcodes,function(p,date){
     #print(p)
    n = mg[[p]]
    current = n[date]
    if(nrow(current)==1 && !is.na(current$volatile))
    {
      i = which(index(n) == date)
      if(i < (nday+1) ) return(NULL)
      if(is.na(n[i-nday]$volatile) || n[i-nday]$volatile < 0) return(NULL)
      #刚好跌倒第n天
      allfall = all(n[(i-nday+1):i]$volatile < 0)
      allfallratio = sum(n[(i-nday+1):i]$volatile)
      
      if(!is.null(shindex))
      {
        j = which(index(shindex) == date)
        allup = all(shindex[(j-nday+1):j]$volatile > 0)
        if(!allup) return(NULL)
      }
      
      if(!is.na(allfall) && !is.na(allfallratio) &&allfall && allfallratio < ratio)
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
}