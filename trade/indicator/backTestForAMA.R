require(quantmod)
require(TTR)
library(blotter)

backTestForAMA<-function(stockdata,n=5,fast=2,slow=10,initEq=1000000,initDate="1990-01-01")
{
  .blotter <<- new.env()
  stockdata$ama=AMA(Cl(stockdata),n,fast,slow)
  
  #初始化账户、头寸等信息
  symbol = c("stockdata")
  currency("USD")
  stock(symbol, currency="USD",multiplier=1)
  
  portfolio = "stockdata"
  initPortf(name=portfolio,symbol, initDate=initDate)
  account = "stockdata"
  initAcct(name=account,portfolios=portfolio, initDate=initDate, initEq=initEq)
  #不用打印成交信息
  verbose=FALSE
  for(i in (n+2):NROW(stockdata))
  {
    #存在ama值时开始计算
    if(!is.na(coredata(stockdata[i,'ama'])))
    {
      CurrentDate=time(stockdata)[i-1]
      equity<-getEndEq(portfolio, CurrentDate)
      Posn <- getPosQty(portfolio, Symbol=symbol, Date=CurrentDate)
      #无资金管理,每次满仓
      ClosePrice <- as.numeric(Cl(stockdata[i-1,]))
      UnitSize <-as.numeric(trunc(equity/ClosePrice))
      
      #持仓为零时，判断是否买入
      if(Posn ==  0)
      {
       
        if(ClosePrice > as.numeric(stockdata[i-1,'ama']))
        {
          addTxn(portfolio, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0,verbose=verbose) 
        }
      }
      else
      {
        if(ClosePrice < as.numeric(stockdata[i-1,'ama']))
        {
          addTxn(portfolio, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
        }
      }
      #更新账户信息
      updatePortf(portfolio, Dates = CurrentDate)
      updateAcct(portfolio, Dates = CurrentDate)
      updateEndEq(portfolio, Dates = CurrentDate)
    }
  }
  return(tradeStats(portfolio,'stockdata'))
}


backTestForMA<-function(stockdata,n=5,initEq=1000000,initDate="1990-01-01")
{
  .blotter <<- new.env()
  stockdata$ma=SMA(Cl(stockdata),n)
  
  #初始化账户、头寸等信息
  symbol = c("stockdata")
  currency("USD")
  stock(symbol, currency="USD",multiplier=1)
  
  portfolio = "stockdata"
  initPortf(name=portfolio,symbol, initDate=initDate)
  account = "stockdata"
  initAcct(name=account,portfolios=portfolio, initDate=initDate, initEq=initEq)
  #不用打印成交信息
  verbose=FALSE
  #遍历每只股票的历史
  for(i in (n+2):NROW(stockdata))
  {
    #存在ama值时开始计算
    if(!is.na(stockdata[i,'ma']))
    {
      CurrentDate=time(stockdata)[i-1]
      equity<-getEndEq(portfolio, CurrentDate)
      Posn <- getPosQty(portfolio, Symbol=symbol, Date=CurrentDate)
      #无资金管理,每次满仓
      ClosePrice <- as.numeric(Cl(stockdata[i-1,]))
      UnitSize <-as.numeric(trunc(equity/ClosePrice))
      
      #持仓为零时，判断是否买入
      if(Posn ==  0)
      {
        #收盘价小于ama，买入
        if(ClosePrice > as.numeric(stockdata[i-1,'ma']))
        {
          addTxn(portfolio, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0,verbose=verbose) 
        }
      }
      else
      {
        #收盘价大于ama，卖出
        if(ClosePrice < as.numeric(stockdata[i-1,'ma']))
        {
          addTxn(portfolio, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
        }
      }
      #更新账户信息
      updatePortf(portfolio, Dates = CurrentDate)
      updateAcct(portfolio, Dates = CurrentDate)
      updateEndEq(portfolio, Dates = CurrentDate)
    }
  }
  return(tradeStats(portfolio,'stockdata'))
}


source.with.encoding('C:/mac/desk/R/code/trade/indicator/AMA.R', encoding='UTF-8', echo=TRUE)
#历史记录表
path="D:/stock/dest"
Sys.setenv(TZ="UTC")
files<-dir(path)
testNum = 10
subset = sample(c(1:length(files)),testNum)
result = list()
n=5
fast = 2
slow = 10
for(i in 1:testNum)
{
  f=files[subset[i]]
  fname<-file.path(path,f)
  #读入数据
  try(stockdata<-read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
  colnames(stockdata)<-c("Open","High","Low","Close","Volume","Amount")
  if(NROW(stockdata) <= n)
  {
    next
  }
  #设置时间格式
  time(stockdata)=as.POSIXct(time(stockdata))
  #转化为xts对象
  stockdata=as.xts(stockdata)
  #测试2005年后的数据
  stockdata=stockdata['2005/']
  
  p_ama=backTestForAMA(stockdata,n=n,fast=fast,slow=slow)
  p_ma=backTestForMA(stockdata,n=n)
  diff_pl = p_ama$Net.Trading.PL - p_ma$Net.Trading.PL
  diff_pw = p_ama$Percent.Positive - p_ma$Percent.Positive
  diff_md = p_ama$Max.Drawdown - p_ma$Max.Drawdown
  result[[i]] = list(diff_pl=diff_pl,diff_pw=diff_pw,diff_md=diff_md,pl = p_ama$Net.Trading.PL,pmd=p_ama$Max.Drawdown)
}

#分析结果
diffpl=sapply(result,function(x){return(x$diff_pl)})
#ama较优的数量和比例
count(diffpl[diffpl>0])
count(diffpl[diffpl>0]) / length(diffpl)

diffpw=sapply(result,function(x){return(x$diff_pw)})
#ama获胜率较优的数量和比例
count(diffpw[diffpw>0])
count(diffpw[diffpw>0]) / length(diffpw)

diffmd=sapply(result,function(x){return(x$diff_md)})
#ama最大回撤较小的数量和比例
count(diffmd[diffmd<0])
count(diffmd[diffmd<0]) / length(diffmd)

#ama盈利
pl = sapply(result,function(x){return(x$pl)})
#大于0的比例
count(diffpl[diffpl>0])
count(diffpl[diffpl>0]) / length(diffpl)
