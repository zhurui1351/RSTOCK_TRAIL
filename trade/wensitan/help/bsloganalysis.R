bsloganalysis = function(records)
{
  shorttrade = subset(records,type=='short')
  longtrade = subset(records,type=='long')
  
  shortprofit = -shorttrade$profit
  longprofit = longtrade$profit
  
  totalshortprofit = sum(shortprofit)
  totallongprofit = sum(longprofit)
  
  totalprofit = totalshortprofit + totallongprofit
  winratio = (length(shortprofit[shortprofit>0]) + length(longprofit[longprofit>0])) / (length(shortprofit) + length(longprofit))
  longwinratio = length(longprofit[longprofit>0]) / length(longprofit)
  shortwinratio = length(shortprofit[shortprofit>0]) / length(shortprofit)
  
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
  
  e = merge(el,es)
  e = merge(e,elwration)
  e = merge(e,eswration)
  e$total = e$profitlong + e$profitshort
}