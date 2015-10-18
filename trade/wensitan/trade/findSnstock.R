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
  
#   slm =  Filter(function(x){ ratio = x[[3]]
#                              month = x[[2]]
#                              ratio>=0.8 && month==5 },lm)
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
    assign(paste('lm',yearnow,sep=''),lm)
    for(i in 1: 12)
    {
      #筛选满足条件的记录
      slm =  Filter(function(x){ ratio = x[[3]]
      month = x[[2]]
      ratio>=0.8 && month==i },lm)
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
          if(nrow(tradeinfo)==0) next
          enter = as.numeric(Op(tradeinfo))
          out = as.numeric(Cl(tradeinfo))
          record = data.frame(code=code,opdate=paste(yearnow,m,sep=''),cldate=paste(yearnow,m,sep=''),Open=enter,Close=out,profit=as.numeric(out-enter),initStop=0,stopprice=0,type='clean')
          records = rbind(records,record)
        }
      }
    }
  }
  
  profit = as.numeric(records[,'profit'])
  sum(profit)
  max(profit)
  min(profit)
  length(profit[profit>0]) / length(profit)
  everyyear = substr(records[,'opdate'],1,4)
  aggregate(x=profit,by=list(everyyear),sum)
  sub = subset(records,substr(opdate,1,4)== '2008' )
  aggregate(x=sub[,'profit'],by=list(sub[,'opdate']),function(x)(length(x[x>0])/length(x)))
  x=aggregate(profit~opdate,data=records[,c('opdate','profit')],function(x){n=sample(1:length(x),1)
                                                         return(x[n])})
  uniquedate =  as.character(unique(records[,'opdate']))
  subrecords = subset(records,substr(opdate,1,4) == '2015') 
 
  subrecords = subset(records,Open < 25 & substr(code,1,1)!='3') 
  
  randomtrade = lapply(uniquedate,function(x,records){
    subrecords = subset(records,opdate == x)
    nr = nrow(subrecords)
    if(nr <= 5) {return(subrecords)}
    else
    {
      n = sample(1:nrow(subrecords),5)
      return(subrecords[n,])
    }
  },subrecords)
  randomtrade = do.call('rbind',randomtrade)
  anlysisProfit(randomtrade)
}


testinenvir = function()
{
  testdate = as.character(2001:2015)
  records = data.frame()
  for(d in testdate)
  {
    print(d)
    lm = get(paste('lm',d,sep=''))
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
          if(nrow(tradeinfo)==0) next
          enter = as.numeric(Op(tradeinfo))
          out = as.numeric(Cl(tradeinfo))
          record = data.frame(code=code,opdate=paste(d,m,sep=''),cldate=paste(d,m,sep=''),Open=enter,Close=out,profit=as.numeric(out-enter),initStop=0,stopprice=0,type='clean')
          records = rbind(records,record)
        }
      }
    }
  }
}


anlysisProfit = function(records)
{
  profit = as.numeric(records[,'profit'])
  print('total nums:')
  print(length(profit))
  print('total profit:')
  print(sum(profit))
  print('max:')
  print(max(profit))
  print('min:')
  print(min(profit))
  print('win ratio:')
  print( length(profit[profit>0]) / length(profit))
  everyyear = substr(records[,'opdate'],1,4)
  each_profit=aggregate(x=profit,by=list(everyyear),sum)
  each_count=aggregate(x=profit,by=list(everyyear),length)
  each_ratio = aggregate(x=profit,by=list(everyyear),function(x){length(x[x>0])/length(x)})
  each_year= merge(each_profit,each_ratio,by='Group.1')
  each_year = merge(each_year,each_count,by='Group.1')
  names(each_year) = c('year','profit','win ratio','nums')
  print('every year:')
  print(each_year)
}
