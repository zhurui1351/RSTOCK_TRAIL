sortBeststocks = function(daydate,mg)
{
  allcodes = names(mg)
  l = sapply(allcodes,function(p,date){
    print(p)
    n = mg[[p]]
    current = n[date]
    if(nrow(current)==1 && !is.na(current$volatile))
    {
      v = c(as.numeric(current$volatile))
      names(v) = p
      return(v)
    }
    
  }
  ,daydate)
  l=Filter(function(x){!is.null(x)},l)
  lname = names(l)
  l = unlist(l)
  names(l) = NULL
  lname = lname[order(l,decreasing=T)[1:30]]
  lv = l[order(l,decreasing=T)[1:30]]
  return(data.frame(code=lname,votile=lv))
}