findtimeGapWhengrowToSomeDegree = function(pname,pdate)
{
  p = get(pname)
  j = which(index(p) == pdate)
  pdate = as.character(index(p)[j+1])
  if(is.na(pdate)) return(NA)
  enter = as.numeric(Op(p[pdate]))
  if(is.na(enter)) return(NA)
  sep = -1
  for(i in (j+1) : length(index(p)))
  {
    cur = p[i,]
    pre = p[(i-1),]
    if(is.na(Cl(cur))) next
    if(((as.numeric(Cl(cur) - enter)) / as.numeric(enter)) > 0.2)
    {
      sep = i - j
      return(sep)
    }
    
  }
}
