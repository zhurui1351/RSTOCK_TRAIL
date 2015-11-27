uniquelist = function(l)
{
  templist = list()
  j = 1
  prevars = c('')
  for(i in 1 : length(l))
  {
    vars = l[[i]]$vars
    if(setequal(vars,prevars))
    {
      next
    }
    else
    {
      prevars = vars
      templist[[j]] = l[[i]]
      j = j + 1
    }
  }
  return(templist)
}


varset_length = length(varset)
#inital set
indexset = combn(varset,1)
l = apply(indexset, MARGIN = 2, FUN=function(x)
{
  vars = paste0(x,collapse = "+")
  f = formula(paste('leadclflag ~ ',vars))
  precise = getBsModelCriterion(longtotest,f,testdate,pricedata,analysedata)
  return(list(vars = vars,precise = precise,leng =length(x)))
}
)

l = l[order(sapply(l,function(x){x$precise}),decreasing=TRUE)]

bestset = l[1:5]

for(i in 1 : (varset_length - 1))
{
  print(i)
  preset = lapply(bestset, function(x,i){ if(x$leng==i) return(x)},i)
  preset = Filter(function(x){length(x) >0},preset)
  
  templist = list()
  listindex = 1
  for( j in 1:length(preset))
  {
    vars = preset[[j]]$vars
    varsindex = which(varset %in% vars)
    addvarset = varset[-varsindex]
    for( v in addvarset)
    {
      vars_v = c(vars,v)
      vars_v = vars_v[order(vars_v,decreasing = F)]
      vars_str = paste0(vars_v,collapse = "+")
      f = formula(paste('leadclflag ~ ',vars_str))
      precise = getBsModelCriterion(longtotest,f,testdate,pricedata,analysedata)
      templist[[listindex]] = list(vars = vars_v,precise = precise,leng =length(vars_v))
      listindex = listindex + 1
    }
  }
  
  templist = templist[order(sapply(templist,function(x){x$precise}),decreasing=TRUE)]
  templist = uniquelist(templist)
  bestset = append(bestset,templist[1:5])
}

bestset1 = Filter(function(x){length(x) >0},bestset)
bestset = bestset[order(sapply(bestset1,function(x){x$precise}),decreasing=TRUE)]
