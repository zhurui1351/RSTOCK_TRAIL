

AMA<-function(x,n=5,fast=2,slow=10)
{
  getSt <- function(x,n)
  {
    result = rep(NA,n)
    for(i in n:NROW(x))
    {
      result = c(result,as.numeric(x[i]) - as.numeric(x[i-n]))
    }
    return(result)
  }
  x <- try.xts(x, error = as.matrix)
  if (n < 1 || n > NROW(x)) 
    stop("Invalid 'n'")
  if (any(nNonNA <- n > colSums(!is.na(x)))) 
    stop("n > number of non-NA values in column(s) ", paste(which(nNonNA), 
                                                            collapse = ", "))
  x.na <- xts:::naCheck(x, n)
  st = getSt(x,n)
  
  vt  = getSt(x,1)
  vt = runSum(abs(vt),n)
  ert = st/vt
  ct = ert * 2 /(fast+1) + (1-ert) * 2 / (slow+1)
  sct = ct ^ 2
  nna = sum(is.na(sct))
  #初期为前日close价
  ama0 =  x[nna+1] 
  ama = c(rep(NA,nna),ama0)
  for(i in (nna+2) : NROW(sct))
  {
    amai = sct[i]*as.numeric(x[i]) + (1-sct[i]) * ama[i-1]
    ama = c(ama,amai)
  }
  ama = reclass(ama,x)
  return(ama)
}
