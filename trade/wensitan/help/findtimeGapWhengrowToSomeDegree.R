findtimeGapWhengrowToSomeDegree = function(pname,pdate,degree = 0.1)
{
 # print(pname)
  #print(pdate)
  p = get(pname)
  j = which(index(p) == pdate)
  pdate = as.character(index(p)[j+1])
  if(is.na(pdate)) return(NA)
  enter = as.numeric(Op(p[pdate]))
  if(is.na(enter)) return(NA)
  sep = -1
  if((j+2) >= length(index(p))) return(NA)
  for(i in (j+2) : length(index(p)))
  {
   # print(i)
    cur = p[i,]
    if(is.na(Cl(cur)) || Cl(cur) ==0) next
    if(((as.numeric(Cl(cur) - enter)) / as.numeric(enter)) > degree)
    {
      sep = i - j
      return(sep)
    }
  }
  return(sep)
}

findStockWhengrowToSomeDegree = function(pname,pdate,degree = 0.1)
{
  # print(pname)
  #print(pdate)
  p = get(pname)
  j = which(index(p) == pdate)
  pdate = as.character(index(p)[j+1])
  if(is.na(pdate)) return(NA)
  enter = as.numeric(Op(p[pdate]))
  if(is.na(enter)) return(NA)
  sep = NULL
  for(i in (j+2) : length(index(p)))
  {
    cur = p[i,]
    pre = p[(i-1),]
    if(is.na(Cl(cur))) next
    if(((as.numeric(Cl(cur) - enter)) / as.numeric(enter)) > degree)
    {
      sep = i - j
      return(list(pname,pdate,sep))
    }
  }
}

afterNdaysProfit = function(pname,pdate,n=2)
{
  p = get(pname)
  j = which(index(p) == pdate)
  pdate = as.character(index(p)[j+1])
  if(is.na(pdate)) return(NA)
  enter = as.numeric(Op(p[pdate]))
  if(is.na(enter)) return(NA)
  pdate = as.character(index(p)[j+n])
  if(is.na(pdate)) return(NA)
  exit = as.numeric(Cl(p[pdate]))
  if(is.na(exit) || exit==0) return(NA)
  return(exit-enter)
  
}
