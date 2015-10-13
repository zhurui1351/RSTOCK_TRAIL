findSnStock = function()
{
  allcodes = names(mg)
  lm=lapply(allcodes,FUN=function(x){
    print(x)
    l = testMonthPeriod(code=x)
    if(!is.null(l)) return(l)
  })
  
  lm = Filter(function(x){ ll = x[[1]]
                             length(ll)!=0},lm)
  
  slm =  Filter(function(x){ ratio = x[[3]]
                             month = x[[2]]
                             ratio>=0.75 && month==11 },lm)
  
}