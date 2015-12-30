#find bull 
find_bull_first = function(upratio = 1,downratio = -0.3,shindex)
{
  templow =as.numeric(Cl(shindex[1,]))
  temphi = as.numeric(Cl(shindex[1,]))
  from = index(shindex[1,])
  to = index(shindex[1,])
  for(i in 2:nrow(shindex))
  {
    
    cur = shindex[i,]
    curHi = as.numeric(Cl(cur))
    curLo = as.numeric(Cl(cur))
    curdate = index(cur)
    
    templow_1 = templow
    
    if(curLo < templow)
    {
      templow = curLo
      from = curdate
      to = curdate
    }
    
    if( ((curHi-templow_1) / templow_1) > upratio )
    {
      to = curdate
      end = to
      temphi = curHi
      break;
    }
  }
  
  if(i == nrow(shindex))
  {
    return(c(from=from,to=curdate,end = end))
  }
  
  for(j in (i+1) : nrow(shindex))
  {
    cur = shindex[j,]
    curHi = as.numeric(Cl(cur))
    curLo = as.numeric(Cl(cur))
    curdate = index(cur)
    
    temphi_1 = temphi
    
    if(curHi > temphi)
    {
      to = curdate
      temphi = curHi
      end = to
    }
    
    if(((curLo - temphi_1) / temphi_1) < downratio)
    {
      end = curdate
      break
    }
  }
  
  return(c(from=from,to=to,end = end))
  #dygraph(Cl(shindex[paste(from,end,sep='/')]))
}

find_bull = function(upratio = 1,downratio = -0.2,shindex)
{
  i = 1
  l = list()
  li = 1
  while(i < nrow(shindex))
  {
    temp = find_bull_first(upratio,downratio,shindex[i:nrow(shindex),])
    l[[li]] =  temp
    li = li + 1
    
    to = temp['to']
    pos = which(index(shindex) == to)
    
    i = pos+1
  }
  return(l)
}

# find bear
