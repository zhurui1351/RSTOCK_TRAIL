
mult = function(a,b)
{
  return(a*b)
}

minop = function(a)
{
  return(min(1,sum(a)))
}

fuzzyop = function(w1,w2,op=mult)
{
  lw = length(w)
  result = c()
  for(i in 1:lw)
  {
    #r = w1[i]*w2[i]
    #r = min(w1[i],w2[i])
    r = op(w1[i],w2[i])
    result = c(result,r)
  }
  return(result)
}



fuzzy = function(f,w,op=minop)
{
  n = ncol(f)
  result = c()
  for( i in 1:n)
  {
    coli = f[,i]
    r = fuzzyop(w,coli)
    #r = max(r)
    #r = min(1,sum(r))
    r = op(r)
    result = c(result,r) 
  }
  return(result)
}

f1 = c(0,0.035,0.86,0.105,0)
f2 =c(0,0,0.69,0.2,0.1)
f3 = c(0,0,0.6,0.295,0.105)
f4 = c(0,0.01,0.67,0.23,0.09)
f5 = c(0,0,0.565,0.3123,1225)
f=matrix(c(f1,f2,f3,f4,f5),nrow=5,byrow=T)

w = c(0.2,0.2,0.2,0.2,0.2)

result = fuzzy(f,w)
print(result)
