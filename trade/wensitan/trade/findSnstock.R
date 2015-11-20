rm(list=ls(all=T))
require(quantmod)
require(TTR)
require('dygraphs')
require('lubridate')
require('dplyr')

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/help')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/analysis')
source('D:/Rcode/code/RSTOCK_TRAIL/trade/SNPACKAGE/analysis/testMonthPeriod.R')
source('D:/Rcode/code/RSTOCK_TRAIL/scrapdata/stockdata/get.stock.index.info.R')
findSnStock = function(from='1990',to='2014')
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
  testdate = as.character(2001:2015)
  from = '1990'
  for(d in testdate)
  {
    print(d)
    trainyear = as.character(as.numeric(d) - 1)
    #print(trainyear)
    lm = findSnStock(to=trainyear)
    assign(paste('lm',trainyear,sep=''),lm)
    for(i in 1: 12)
    {
      #筛选满足条件的记录
      slm =  Filter(function(x){ ratio = x[[3]]
      month = x[[2]]
      ratio>=0.75 && month==i },lm)
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
          tradeinfo = stock[paste(d,m,sep='')]
          if(nrow(tradeinfo)==0) next
          enter = as.numeric(Op(tradeinfo))
          out = as.numeric(Cl(tradeinfo))
          record = data.frame(code=code,opdate=paste(d,m,sep=''),cldate=paste(d,m,sep=''),Open=enter,Close=out,profit=as.numeric(out-enter),initStop=0,stopprice=0,type='clean')
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
  subrecords = subset(records1,substr(opdate,1,4) == '2015') 
 
  records1 = subset(records,Open < 25 & substr(code,1,1)!='3') 
  
  randomtrade = lapply(uniquedate,function(x,records){
    subrecords = subset(records,opdate == x)
    nr = nrow(subrecords)
    if(nr <= 5) {return(subrecords)}
    else
    {
      n = sample(1:nrow(subrecords),5)
      return(subrecords[n,])
    }
  },records)
  randomtrade = do.call('rbind',randomtrade)
  anlysisProfit(randomtrade,aggregatecontrol = 4)
}


testinenvir = function()
{
  testdate = as.character(2001:2015)
  records = data.frame()
  for(d in testdate)
  {
    print(d)
    trainyear = as.character(as.numeric(d) - 1)
    lm = get(paste('lm',trainyear,sep=''))
    for(i in 1: 12)
    {
      #筛选满足条件的记录
      slm =  Filter(function(x){ ratio = x[[3]]
                                 month = x[[2]]
                                 ratio>=0.75 && month==i },lm)
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
          tradeinfo = stock[paste(d,m,sep='')]
          if(nrow(tradeinfo)==0) next
          enter = as.numeric(Op(tradeinfo))
          out = as.numeric(Cl(tradeinfo))
          high = as.numeric(Hi(tradeinfo))
          low = as.numeric(Lo(tradeinfo))
          record = data.frame(code=code,opdate=paste(d,m,sep=''),cldate=paste(d,m,sep=''),Open=enter,Close=out,high=high,low=low,profit=as.numeric(out-enter),initStop=0,stopprice=0,type='clean')
          records = rbind(records,record)
        }
      }
    }
  }
}


anlysisProfit = function(records,aggregatecontrol=4)
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
  everyyear = substr(records[,'opdate'],1,aggregatecontrol)
  each_profit=aggregate(x=profit,by=list(everyyear),sum)
  each_count=aggregate(x=profit,by=list(everyyear),length)
  each_ratio = aggregate(x=profit,by=list(everyyear),function(x){length(x[x>0])/length(x)})
  each_year= merge(each_profit,each_ratio,by='Group.1')
  each_year = merge(each_year,each_count,by='Group.1')
  names(each_year) = c('year','profit','win ratio','nums')
  print('every year:')
  print(each_year)
}



allcodes = readallpuredata(period='years')
mg = mget(allcodes)

shindex = readSHindex()
shindex_week = to.weekly(shindex)
shindex_week$sma30 = SMA(Cl(shindex_week),n=30)
shindex_week = na.omit(shindex_week)
shindex_week$volatile = (Cl(shindex_week)-Op(shindex_week))/Op(shindex_week)

shindex_week$stage = judegeStage(shindex_week$sma30)

lm = findSnStock(from='1990',to='2014')

slm =  Filter(function(x){ ratio = x[[3]]
month = x[[2]]
ratio>=0.75 && month==11 },lm)

#计算相关系数
codes = sapply(slm,function(x){x$code})
l=lapply(codes, function(x){
  p = readOneStock(x)
  p = to.monthly(p)
  p =Delt(Cl(p))
  p = p['2012/2015']
  return(p)
})

names(l) = codes
m = do.call('cbind',l)
colnames(m) = codes

mcor = cor(m,use='na.or.complete')
mcor[lower.tri(mcor)] = 1
msort=sort(as.vector(mcor))[1:10]
l=lapply(msort, function(x){which(mcor==x,arr.ind = T)})

code1=rownames(mcor)[l[[4]][1]]
code2=colnames(mcor)[l[[4]][2]]

testMonthPeriod(code=code1,from='1990',to='2014',detail = T)
testMonthPeriod(code=code2,from='1990',to='2014',detail = T)




corbewteenstock = function(xx)
{
  xx =  Filter(function(x){ !is.null(x)},xx)
  xcor = lapply(xx,function(x,code){
    xc = readOneStock(code)
    xc = to.monthly(xc)
    xc =Delt(Cl(xc))
    xc = xc['2012/2015']
    
    x1 = readOneStock(x)
    x1 = to.monthly(x1)
    x1 =Delt(Cl(x1))
    x1 = x1['2012/2015']
    
    co =  cor(cbind(xc,x1),use='na.or.complete')
    return(c(x,co[2,1]))
  },'600961')
}


#600234
#000971
testMonthPeriod(code='600618',from='1990',to='2014',detail = T)
testMonthPeriod(code='600995',from='1990',to='2014',detail = T)

getSymbols('600618.SS')
getSymbols('000971.SZ')

get.stock.info("sh600618")

code = '000971'
code_yh = ifelse(substr(code,1,1) == '6',paste(code,'SS',sep='.'),paste(code,'SZ',sep='.'))
code_sina = ifelse(substr(code,1,1) == '6',paste('sh',code,sep=''),paste('sz',code,sep=''))



monitormonth = function(codes)
{
  tt = sapply(codes, function(x){
    code = x
    code_yh = ifelse(substr(code,1,1) == '6',paste(code,'SS',sep='.'),paste(code,'SZ',sep='.'))
    code_sina = ifelse(substr(code,1,1) == '6',paste('sh',code,sep=''),paste('sz',code,sep=''))
    e = parent.env(environment())
    if(!exists(code_yh))
    {
      p = suppressWarnings(getSymbols(code_yh,from='1990-01-01',auto.assign = F))
      p = adjustOHLC(p,use.Adjusted = T)
      assign(code_yh,p,envir = e)
      return(code)
    }
  })
  while(T)
  {
    date = '20151109'
    datem = substr(date,1,6)

    for(code in codes)
    {
      print(code)
      
      code_yh = ifelse(substr(code,1,1) == '6',paste(code,'SS',sep='.'),paste(code,'SZ',sep='.'))
      code_sina = ifelse(substr(code,1,1) == '6',paste('sh',code,sep=''),paste('sz',code,sep=''))
      p = get(code_yh)
      pm = to.monthly(p)
      pm =pm[datem]
      p$sma3 = SMA(Cl(p),3)
      p$sma5 = SMA(Cl(p),5)
      p$sma30 = SMA(Cl(p),30)
      
      pd = get.stock.info(code_sina)
      pd$monthopen = Op(pd)
      pd$monthhigh = Hi(pd)
      pd$monthlow = Lo(pd)
      pd$sma30 = p$sma30[date]
      pd$sma5 = p$sma5[date]
      pd$sma3 = p$sma3[date]
      print(pd[,c(1:6,33:38)])
    }
    Sys.sleep(15)
    
  }
}

monitorinmonth = function(slm)
{
  xx = lapply(slm, function(x)
  {
    code = x$code
    # print(code)
    p = readOneStock(code)
    p = to.monthly(p)
    p = p['201511']
    if(nrow(p) != 0)
    {
      if(as.numeric(Cl(p) - Op(p)) < 0)
      {
        print(code)
        print(OHLC(p))
        return(code)
      }
    }
    
  }
  )
}