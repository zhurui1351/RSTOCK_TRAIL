#pricedata = readOneStock('600390')

testMonthPeriod = function(pricedata=NULL,code='',from='1990',to='2015',detail=F)
{
  require(lubridate)
  if(is.null(pricedata))
    pricedata = readOneStock(code)
  pricedata = pricedata[paste(from,to,sep='/')]
  if(is.null(pricedata) || nrow(pricedata)==0) return(NULL)
  pricedata_m = to.period(pricedata,'months')
  if(nrow(pricedata_m) < 120) 
  {
    warning('no enough data')
    return(NULL)
  } 
  pricedata_m$month = month(pricedata_m)
  pricedata_m$votile = Cl(pricedata_m) - Op(pricedata_m)
  xa = as.vector(pricedata_m[,'month'])
  xb = as.vector(pricedata_m[,'votile'])
  xc = data.frame(xa,xb)
  result = aggregate(xb~xa,data=xc,FUN=function(x)(length(x[x>0]) / length(x)))
  maxmonth = subset(result,xb==max(xb))
  minmonth = subset(result,xb==min(xb))
  
  if(detail)
  {
    print(result)
    print(subset(pricedata_m,month==maxmonth[1,1]))
    print('short:')
    print(subset(pricedata_m,month==minmonth[1,1]))
    
  }
  # sum(subset(pricedata_m,month==maxmonth[1,1],select='votile'))
  return(list(code=code,month=maxmonth[1,1],ratio=maxmonth[1,2],minmonth=minmonth[1,1],ratio=minmonth[1,2]))
}

#rules为list 为单独日期及其组合
testOneSnrule = function(mydata,confProp=0.6,conf=0.05,rules,tradeDays=5,type='iw')
{
  stockdata = initialData(stockdata,type=type)
  base_prob = getBasePropByRules(rules,stockdata,tradeDays)
  condition_rules = getConditionRules(base_prob)
  print(base_prob)
  print(condition_rules)
  return(interestingRules)
}
