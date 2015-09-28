afterNatrExit = function(pname,pdate,initStopRatio=2,stepratio=0.5,stopratio=0.5)
{
  p = get(pname)
  p = subset(p,!is.na(Open) & !is.na(Close))
  j = which(index(p) == pdate)
  enterdate = as.character(index(p)[j+1])
  if(is.na(enterdate)) return(NA)
  enterp = p[enterdate]
  
  enter = as.numeric(enterp[,'High'])
  if(is.na(enter)) return(NA)
  
  atr = as.numeric(p[pdate]$atr)
  if(is.na(atr)) return(NA)
  
  initstopprice = enter - initStopRatio*atr
  stopprice = initstopprice
  
  if((j+2) >= length(index(p))) return(NA)
  
  startTrail = enter
  for(i in (j+2) : length(index(p)))
  {
    # print(i)
    cur = p[i,]
    if(is.na(Cl(cur)) || Cl(cur) ==0) next
    
    if(as.numeric(Cl(cur)) < stopprice )
    { 
      if(i == length(index(p))) next
      i = i + 1
      closedate = as.character(index(p)[i])
      cur = p[i,]
      record = list(code=pname,opdate=enterdate,cldate=closedate,Open=enter,Close=as.numeric(Lo(cur)),profit=as.numeric(Lo(cur))-enter,initStop=initstopprice,stopprice=stopprice,type='clean')
    #  print(i)
      return(record)
    }
    
    if( (as.numeric(cur[,'Close'])-startTrail) > stepratio * atr )
    {
     # print(i)
      startTrail = as.numeric(Cl(cur))
      atr = as.numeric(cur$atr)
      stopprice = as.numeric(Cl(cur)) - stopratio*atr
      
    }
  }
  closedate = as.character(index(p)[i])
  record = list(code=pname,opdate=enterdate,cldate=closedate,Open=enter,Close=as.numeric(Cl(cur)),profit=as.numeric(Lo(cur))-enter,initStop=initstopprice,stopprice=stopprice,type='float')
  return(record)
}