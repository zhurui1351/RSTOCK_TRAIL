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
  testdate = as.character(2000:2015)
  from = '1990'
  for(d in testdate)
  {
    print(d)
    trainyear = as.character(as.numeric(d) - 1)
    print(trainyear)
    lm = findSnStock(to=trainyear)
    assign(paste('lm',trainyear,sep=''),lm)
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
          
          stock_day = readOneStock(code)
          stock = to.monthly(stock_day)
          stock_day$sma5 = lag(SMA(stock_day$Close,5))
          stock_day$sma10 = lag(SMA(stock_day$Close,10))
          stock_day$sma3 = lag(SMA(stock_day$Close,3))
          stock_day$preclose = lag(stock_day$Close)
          tradeinfo = stock[paste(d,m,sep='')]
          if(nrow(tradeinfo)==0) next
          enter = as.numeric(Op(tradeinfo))
          out = as.numeric(Cl(tradeinfo))
          #######
          perd = paste(d,m,sep='')
          stocks = stock_day[perd]
          enterflag = T
          outflag = T
          stopprice = 0
          initstop = 0
          for(testi in 1:nrow(stocks))
          {
            cur = stocks[testi,]
            if(!enterflag)
            {
              if(cur$sma3 > cur$sma10 && cur$preclose > cur$sma3 )
              {
                enter = as.numeric(Op(cur))
                enterflag = T
              }
            }
            if(!outflag && enterflag)
            {
              if( ((as.numeric(cur$Close) - enter) / enter) < -0.1 )
              {
                out = as.numeric(cur$Close)
                outflag  =T
                stopprice = out
              }
            }
          }
          #######
          if(enterflag == T)
          {
            record = data.frame(code=code,opdate=paste(d,m,sep=''),cldate=paste(d,m,sep=''),Open=enter,Close=out,profit=as.numeric(out-enter),initStop=0,stopprice=stopprice, type='clean')
            records = rbind(records,record)
          }
          
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
  subrecords = subset(records,substr(opdate,1,4) == '2011') 
 
  records1 = subset(records,Open < 25 & substr(code,1,1)!='3') 
  
  randomtrade = lapply(uniquedate,function(x,records){
    subrecords = subset(records,opdate == x)
    nr = nrow(subrecords)
    if(nr <= 4) {return(subrecords)}
    else
    {
      n = sample(1:nrow(subrecords),4)
      return(subrecords[n,])
    }
  },records)
  randomtrade = do.call('rbind',randomtrade)
  anlysisProfit(randomtrade,aggregatecontrol = 4)
  
}


testinenvir = function()
{
  testdate = as.character(2006:2015)
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
                                 ratio>=0.8 && month==i },lm)
      
     
      slm = slm[order(sapply(slm,function(x){x[['ratio']]}),decreasing = T)]
      
    if(length(slm)!=0)
      {
        len = ifelse(length(slm) > 4,4,length(slm))
        #len = length(slm)
        #生成测试记录
        for(j in 1 : len)
        {
          info = slm[[j]]
          code = info[['code']]
          m = as.character(info[['month']])
          
          stock_day = readOneStock(code)
          stock = to.monthly(stock_day)
          stock_day$sma5 = lag(SMA(stock_day$Close,5))
          stock_day$sma10 = lag(SMA(stock_day$Close,10))
          stock_day$sma3 = lag(SMA(stock_day$Close,3))
          stock_day$sma30 = lag(SMA(stock_day$Close,30))
          
          stock_day$preclose = lag(stock_day$Close)
          tradeinfo = stock[paste(d,m,sep='')]
          if(nrow(tradeinfo)==0) next
          enter = as.numeric(Op(tradeinfo))
          out = as.numeric(Cl(tradeinfo))
          
          high = as.numeric(Hi(tradeinfo))
          low = as.numeric(Lo(tradeinfo))
          #######
          perd = paste(d,m,sep='')
          stocks = stock_day[perd]
          enterflag = F
          outflag = F
          stopprice = 0
          initstop = 0
          opdate = perd
          cldate = perd
          quant = floor(100/enter)
          if(nrow(stocks) < 15) next
          #前五个交易日买入 ，否则不买入
          for(testi in 1:5)#nrow(stocks))
          {
            cur = stocks[testi,]
            if(!enterflag)
            {
              if(cur$preclose > cur$sma30 )
              {
                enter = as.numeric(Op(cur))
                enterflag = T
                opdate = as.Date(index(cur))
                quant = floor(100 / enter)
                break
              }
            }
            else
            {
              break
            }
          }
          #止损
          stopprice_move = enter-enter*0.1
          ismoved = F
          ismovedout = F
          enddate = as.character(index(stocks[(testi+1),]))
          for(testi1 in as.character(index(stock_day[paste(enddate,'/',sep='')])))
         # for(testi1 in (testi+1):nrow(stocks))
          {
           # cur = stocks[testi1,]
            cur = stock_day[testi1]
            if(!outflag && enterflag)
            {
              if( as.numeric(cur$Close) <= stopprice_move )
              {
                out = as.numeric(cur$Close)
                outflag  =T
                stopprice = out
                if(ismoved) ismovedout = T
                cldate = as.character(as.Date(index(cur)))
                break
              }
            }
            if(as.numeric(cur$High) >= 0.1 * stopprice_move + stopprice_move)
            {
              stopprice_move = stopprice_move + stopprice_move * 0.05
            }
          }
          #######
          if(enterflag == T)
          {
            out = ifelse(ismoved && !ismovedout,stopprice,out)
            record = data.frame(code=code,opdate=opdate,cldate=cldate,Open=enter,Close=out,high=high,low=low,quant=quant,profit=quant * as.numeric(out-enter),initStop=0,stopprice=stopprice,type='clean')
            records = rbind(records,record)
          }
          
        }
      }
    }
  }
}

testinenvir_diffseq = function()
{
  testdate = as.character(2006:2015)
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
      
      
      slm = slm[order(sapply(slm,function(x){x[['ratio']]}),decreasing = T)]
      holdnum = 0 
      holdcodes = c()
      #前五个交易日买入
      for(testi in 1:10)
      {
        #持仓个数

        if(length(slm) != 0 )
        {
          len = length(slm)
          #优先买入符合条件的
          for(j in 1 : len)
          {
            if(holdnum > 10) break
            info = slm[[j]]
            code = info[['code']]
            m = as.character(info[['month']])
            
            if(is.element(code,holdcodes)) next
            
            stock_day = readOneStock(code)
            stock = to.monthly(stock_day)
            stock_day$sma5 = lag(SMA(stock_day$Close,5))
            stock_day$sma10 = lag(SMA(stock_day$Close,10))
            stock_day$sma3 = lag(SMA(stock_day$Close,3))
            stock_day$sma30 = lag(SMA(stock_day$Close,30))
            
            stock_day$preclose = lag(stock_day$Close)
            tradeinfo = stock[paste(d,m,sep='')]
            if(nrow(tradeinfo)==0) next
            enter = as.numeric(Op(tradeinfo))
            out = as.numeric(Cl(tradeinfo))
            
            high = as.numeric(Hi(tradeinfo))
            low = as.numeric(Lo(tradeinfo))

            perd = paste(d,m,sep='')
            stocks = stock_day[perd]
            if(nrow(stocks) < 15) next
            enterflag = F
            outflag = F
            stopprice = 0
            initstop = 0
            opdate = ''
            cldate = ''
            quant = 0
            
            cur = stocks[testi,]
            if(!enterflag)
            {
              if(cur$preclose > cur$sma30 )
              {
                enter = as.numeric(Op(cur))
                enterflag = T
                opdate = as.Date(index(cur))
                quant = floor(100 / enter)
                holdnum = holdnum + 1
                holdcodes = c(holdcodes,code)
                # 考虑何时出场
                stopprice_move = enter-enter*0.1
                ismoved = F
                ismovedout = F
                
                enddate = as.character(index(stocks[(testi+1),]))
                for(testi1 in as.character(index(stock_day[paste(enddate,'/',sep='')])))
                {
                  cur = stock_day[testi1]
                 
                    if( as.numeric(cur$Close) <= stopprice_move )
                    {
                      out = as.numeric(cur$Close)
                      outflag  =T
                      stopprice = out
                      if(ismoved) ismovedout = T
                      cldate = as.character(as.Date(index(cur)))
                      break
                    }
                  
                  if(as.numeric(cur$High) >= 0.1 * stopprice_move + stopprice_move)
                  {
                    stopprice_move = stopprice_move + stopprice_move * 0.08
                  }
                }
              }
              
              if(enterflag == T)
              {
                out = ifelse(ismoved && !ismovedout,stopprice,out)
                record = data.frame(code=code,opdate=opdate,cldate=cldate,Open=enter,Close=out,high=high,low=low,quant=quant,profit=quant * as.numeric(out-enter),initStop=0,stopprice=stopprice,type='clean')
                records = rbind(records,record)
              }
              
            }
            
          }
        }
      }
    }
  }
}
#####
testinenvir_cor = function()
{
  testdate = as.character(2006:2015)
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
      
      slm = slm[order(sapply(slm,function(x){x[['ratio']]}),decreasing = T)]
      if(length(slm) > 4)
      {
        lcor =  unlist(corlist(slm,d))
      }
      else
      {
        lcor = sapply(slm,function(x){x$code})
      }
      
      for(code in lcor)
      {
        stock_day = readOneStock(code)
        stock = to.monthly(stock_day)
        
        stock_day$sma30 = lag(SMA(stock_day$Close,30))
        stock_day$preclose = lag(stock_day$Close)
        
        tradeinfo = stock[paste(d,i,sep='')]
        if(nrow(tradeinfo)==0) next
        enter = as.numeric(Op(tradeinfo))
        out = as.numeric(Cl(tradeinfo))
        high = as.numeric(Hi(tradeinfo))
        low = as.numeric(Lo(tradeinfo))
        opdate = paste(d,i,sep='')
        cldate = paste(d,i,sep='')
        quant = 100 / enter
        
        record = data.frame(code=code,opdate=opdate,cldate=cldate,Open=enter,Close=out,high=high,low=low,quant=quant,profit=quant * as.numeric(out-enter),initStop=0,stopprice=stopprice,type='clean')
        records = rbind(records,record)
        
      }
      
    }
  }
}


#相关系数函数
corlist = function(slm,cury)
{
  codes = sapply(slm,function(x){x$code})
  prey = as.numeric(cury) - 3
  l=lapply(codes, function(x){
    p = readOneStock(x)
    p = to.monthly(p)
    p =Delt(Cl(p))
    p = p[paste(prey,cury,sep = '/')]
    return(p)
  })
  names(l) = codes
  m = do.call('cbind',l)
  colnames(m) = codes
  mcor = cor(m,use='na.or.complete')
  mcor[lower.tri(mcor)] = 1
  msort=sort(as.vector(abs(mcor)))[1:2]
  l=lapply(msort, function(x){which(abs(mcor)==x,arr.ind = T)})
  # code1=rownames(mcor)[l[[1]][1]]
  #code2=colnames(mcor)[l[[1]][2]]
  l = lapply(l,function(x){
    code1 = rownames(mcor)[x[1]]
    code2 = rownames(mcor)[x[2]]
    return(c(code1,code2))
  })
  return(l)
}

anlysisProfit = function(records,aggregatecontrol=4,ratio=1)
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

lm = findSnStock(from='1990',to='2015')

slm =  Filter(function(x){ ratio = x[[3]]
month = x[[2]]
ratio>=0.8 && month==1 },lm)

lbest = slm[order(sapply(slm,function(x){x$ratio}),decreasing=TRUE)]



#计算相关系数
codes = sapply(lbest,function(x){x$code})

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

code1=rownames(mcor)[l[[1]][1]]
code2=colnames(mcor)[l[[1]][2]]

testMonthPeriod(code='600525',from='1990',to='2015',detail = T)
testMonthPeriod(code='000039',from='1990',to='2015',detail = T)
get.stock.info('sz000039')
get.stock.info('sh600525')


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

#code_yh = ifelse(substr(code,1,1) == '6',paste(code,'SS',sep='.'),paste(code,'SZ',sep='.'))
#code_sina = ifelse(substr(code,1,1) == '6',paste('sh',code,sep=''),paste('sz',code,sep=''))


#codes = c('600618')
monitormonth = function(codes)
{
  tt = sapply(codes, function(x){
    code = x
    code_yh = ifelse(substr(code,1,1) == '6',paste(code,'SS',sep='.'),paste(code,'SZ',sep='.'))
    code_sina = ifelse(substr(code,1,1) == '6',paste('sh',code,sep=''),paste('sz',code,sep=''))
    e = parent.env(environment())
    rm(list = code_yh,envir=e)
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
    date = '20160111'
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
      pd$monthopen = Op(pm)
      pd$monthhigh = Hi(pm)
      pd$monthlow = Lo(pm)
      pd$sma30 = tail(p$sma30,1)
      pd$sma5 = tail(p$sma5,1)
      pd$sma3 =  tail(p$sma3,1)
      print(pd[,c(1:6,33:38)])
    }
    Sys.sleep(30)
    
  }
}

monitorinmonth = function(codes)
{
 
  date = '20151223'
  datem = substr(date,1,6) 
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
  xx = lapply(codes, function(x)
  {
    code = x
    # print(code)
    #p = readOneStock(code)
    code_yh = ifelse(substr(code,1,1) == '6',paste(code,'SS',sep='.'),paste(code,'SZ',sep='.'))
    code_sina = ifelse(substr(code,1,1) == '6',paste('sh',code,sep=''),paste('sz',code,sep=''))
    p = get(code_yh)
    pm = to.monthly(p)
    pm = pm[datem]
    if(nrow(pm) != 0)
    {
      if(as.numeric(Cl(pm) - Op(pm)) < 0)
      {
        print(code)
        print(OHLC(pm))
        return(code)
      }
    }
    
  }
  )
}