#find bull 
find_bull_first = function(upratio = 1,downratio = -0.3,shindex)
{
  templow =as.numeric(Lo(shindex[1,]))
  temphi = as.numeric(Hi(shindex[1,]))
  from = index(shindex[1,])
  to = index(shindex[1,])
  for(i in 2:nrow(shindex))
  {
    
    cur = shindex[i,]
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
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
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
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

find_bull = function(upratio = 1,downratio = -0.3,shindex)
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

#find bear market

#find bear
find_bear_first = function(upratio = 0.2,downratio = -0.4,shindex)
{
  templow =as.numeric(Lo(shindex[1,]))
  temphi = as.numeric(Hi(shindex[1,]))
  from = index(shindex[1,])
  to = index(shindex[1,])
  for(i in 2:nrow(shindex))
  {
    
    cur = shindex[i,]
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
    curdate = index(cur)
    
    temphigh_1 = temphi
    
    if(curHi > temphi)
    {
      temphi = curHi
      from = curdate
      to = curdate
    }
    
    if( ((curLo-temphigh_1) / temphigh_1) < downratio )
    {
      to = curdate
      end = to
      templow = curLo
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
    curHi = as.numeric(Hi(cur))
    curLo = as.numeric(Lo(cur))
    curdate = index(cur)
    
    templow_1 = templow
    
    if(curLo < templow)
    {
      to = curdate
      templow = curLo
      end = to
    }
    
    if(((curHi - templow_1) / templow_1) > upratio)
    {
      end = curdate
      break
    }
  }
  
  return(c(from=from,to=to,end = end))
  #dygraph(Cl(shindex[paste(from,end,sep='/')]))
}


find_bear = function(upratio = 0.2,downratio = -0.4,shindex)
{
  i = 1
  l = list()
  li = 1
  while(i < nrow(shindex))
  {
    temp = find_bear_first(upratio,downratio,shindex[i:nrow(shindex),])
    l[[li]] =  temp
    li = li + 1
    
    to = temp['to']
    pos = which(index(shindex) == to)
    
    i = pos+1
  }
  return(l)
}

# manul flag bear and bull
flag_for_shindex = function(shindex)
{
  #上涨100%，跌幅 30%
  bear = list(c(from = '1991-01-02',to='1992-05-26'),c(from = '1992-11-17',to='1993-02-16'),c(from = '1994-07-29',to='1994-09-13')
              ,c(from = '1996-01-19',to='1997-05-12'),c(from = '1999-05-19',to='2001-06-14')
              ,c(from = '2005-06-06',to='2007-10-16'),c(from = '2008-10-28',to='2009-08-04'),c(from = '2014-07-22',to='2015-06-12'))
  
  bull = list(c(from = '1992-05-26',to='1992-11-17'),c(from = '1993-02-16',to='1994-07-29'),c(from = '1994-09-13',to='1996-01-19')
              ,c(from = '1997-05-12',to='1999-05-18'),c(from = '2001-06-14',to='2005-06-06'),c(from = '2007-10-16',to='2008-10-28')
              ,c(from = '2009-08-04',to='2010-07-02'))
  
}
