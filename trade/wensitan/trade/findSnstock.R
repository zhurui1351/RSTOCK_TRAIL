findSnStock = function(from='1990',to='2015')
{
  allcodes = names(mg)
  lm=lapply(allcodes,FUN=function(x){
   # print(x)
    l = testMonthPeriod(code=x,from=from,to=to)
    if(!is.null(l)) return(l)
  })
  
  lm = Filter(function(x){ ll = x[[1]]
                             length(ll)!=0},lm)
  
  slm =  Filter(function(x){ ratio = x[[3]]
                             month = x[[2]]
                             ratio>=0.8 && month==5 },lm)
  return(lm)
  
}


snTestFrame = function()
{
  records = data.frame()
  testdate = as.character(2000:2014)
  from = '1990'
  for(d in testdate)
  {
    #print(d)
    yearnow = as.character(as.numeric(d) + 1)
    print(yearnow)
    lm = findSnStock(to=d)
    for(i in 1: 12)
    {
      #筛选满足条件的记录
      slm =  Filter(function(x){ ratio = x[[3]]
      month = x[[2]]
      ratio>=0.7 && month==i },lm)
      if(length(slm)!=0)
      {
        #生成测试记录
        for(j in 1 : length(slm))
        {
          info = slm[[j]]
          code = info[['code']]
          m = as.character(info[['month']])
          
          stock = readOneStock(code)
          stock = to.monthly(stock)
          tradeinfo = stock[paste(yearnow,m,sep='')]
          enter = as.numeric(Op(tradeinfo))
          out = as.numeric(Cl(tradeinfo))
          record = data.frame(code=code,opdate=paste(yearnow,m,sep=''),cldate=paste(yearnow,m,sep=''),Open=enter,Close=out,profit=as.numeric(out-enter),initStop=0,stopprice=0,type='clean')
          records = rbind(records,record)
        }
      }
    }
  }
}