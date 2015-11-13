bsloganalysis = function(records)
{
  shorttrade = subset(records,type=='short')
  longtrade = subset(records,type=='long')
  
  shortprofit = -shorttrade$profit
  longprofit = longtrade$profit
  
  
  
  totalshortprofit = sum(shortprofit)
  totallongprofit = sum(longprofit)
  totalprofit = totalshortprofit + totallongprofit
  print('short profit:')
  print(totalshortprofit)
  print('long profit:')
  print(totallongprofit)
  print('total profit:')
  print(totalprofit)
  
  winratio = (length(shortprofit[shortprofit>0]) + length(longprofit[longprofit>0])) / (length(shortprofit) + length(longprofit))
  longwinratio = length(longprofit[longprofit>0]) / length(longprofit)
  shortwinratio = length(shortprofit[shortprofit>0]) / length(shortprofit)
  print('long win ratio:')
  print(longwinratio)
  print('short win ratio:')
  print(shortwinratio)
  print('total win ratio')
  print(winratio)
  
  print('long nums:')
  print(length(longprofit))
  print('short nums:')
  print(length(shortprofit))
  print('total nums:')
  print(nrow(records))
  
  everyyearlong = substr(longtrade[,'opdate'],1,4)
  el = aggregate(x=longprofit,by=list(everyyearlong),sum)
  colnames(el) = c('year','profitlong')
  
  elwration = aggregate(x=longprofit,by=list(everyyearlong),function(x){length(x[x>0])/length(x)})
  colnames(elwration) = c('year','longwinratio')
  
  everyyearshort = substr(shorttrade[,'opdate'],1,4)
  es = aggregate(x=shortprofit,by=list(everyyearshort),sum)
  colnames(es) = c('year','profitshort')
  
  eswration = aggregate(x=shortprofit,by=list(everyyearshort),function(x){length(x[x>0])/length(x)})
  colnames(eswration) = c('year','shortwinratio')
  
  elnum = aggregate(x=longprofit,by=list(everyyearlong),function(x){length(x)})
  esnum = aggregate(x=shortprofit,by=list(everyyearshort),function(x){length(x)})
  colnames(elnum) = c('year','longnum')
  colnames(esnum) = c('year','shortnum')
  
  
  e = merge(el,es)
  e = merge(e,elwration)
  e = merge(e,elnum)
  e = merge(e,eswration)
  e = merge(e,esnum)
  e$total = e$profitlong + e$profitshort
  print(e)
}

profitjudge = function(records)
{
  if(nrow(records) == 0)
  {
   # print('no records')
    return(F)
    
  }
  shorttrade = subset(records,type=='short')
  longtrade = subset(records,type=='long')
  
  shortprofit = -shorttrade$profit
  longprofit = longtrade$profit
  
  if(nrow(longtrade) > 0)
  {
    everyyearlong = substr(longtrade[,'opdate'],1,4)
    el = aggregate(x=longprofit,by=list(everyyearlong),sum)
    colnames(el) = c('year','profitlong')
  }
  else
  {
    el = 0
  }
  
  if(nrow(shorttrade) > 0)
  {
    everyyearshort = substr(shorttrade[,'opdate'],1,4)
    es = aggregate(x=shortprofit,by=list(everyyearshort),sum)
    colnames(es) = c('year','profitshort')
  }
  else
  {
    es = 0
  }
  
  if(nrow(longtrade) == 0)
  {
     e = merge(es,el)
     colnames(e) = c('year','profitshort','profitlong')
     
  }
  else
  {
    e = merge(el,es)
    colnames(e) = c('year','profitlong','profitshort')
  }
 
  e$total = e$profitlong + e$profitshort
  
#   if(sum(es[,'profitshort'] < 0) > 0)
#   {
#     print(es)
#     return(FALSE)
#   }
#   
#   if(sum(el[,'profitlong'] < 0) > 0)
#   {
#     print(el)
#     return(FALSE)
#   }
  
  if(sum(e[,'total'] < 0) > 2)
  {
  #  print(e)
    return(FALSE)
  }
  
  return(TRUE)
}