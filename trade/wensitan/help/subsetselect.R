
varset_length = length(varset)

for(i in 1 : varset_length)
{
  indexset = combn(varset,1)
  l = apply(indexset, MARGIN = 2, FUN=function(x)
  {
    vars = paste0(x,collapse = "+")
    f = formula(paste('leadclflag ~ ',vars))
    precise = getBsModelCriterion(longtotest,f,testdate,pricedata,analysedata,start,end)
    return(list(vars = vars,precise = precise))
  }
    )
  
  l[order(sapply(l,function(x){x$precise}),decreasing=TRUE)]
  l = l[1:5]
}