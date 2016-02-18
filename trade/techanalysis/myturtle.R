require(quantmod)
require(TTR)
library(blotter)
library(PerformanceAnalytics)
history<-list()
#创建函数保存过程???
updateStrat <- function(Portfolio, Symbol, TxnDate,
                        PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)
{
  NewTxn = xts(t(c(PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)), order.by=as.POSIXct(TxnDate))
  ss <- get('strat')
  ss$strat <- rbind(ss$strat, NewTxn)
  assign('strat',ss)
}
#path="/home/zhu/Desktop/stock/data/stock"
path="D:/stock/dest"
Sys.setenv(TZ="UTC")
files<-dir(path)
files=c('SZ000415.TXT')
#f = files[100]
for(f in files)
{
  print(f)
  fname<-file.path(path,f)
  try(rm("strat"),silent=TRUE)
  try(rm("account.turtles","portfolio.turtles",pos=.blotter),silent=TRUE)
  try(rm("portfolio","account","N",
         "symbol","ClosePrice","CurrentDate",
         "equity","Units","maxUnits","size","Stop","equity",
         "TxnPrice","initDate","initEq","Posn","verbose"),silent=TRUE)
  # 设定初始???
  initDate="2000-01-01"
  initEq=5000
  
  # 投资组合参数
  #size = 0.01
  size=1
  maxUnits = 4
  Units=0
  verbose=TRUE
  try(stockdata<-read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
  colnames(stockdata)<-c("Open","High","Low","Close","Volume","Amount")
  time(stockdata)=as.POSIXct(time(stockdata))
  stockdata=as.xts(stockdata)
  strat<-list()
  symbol = c("stockdata")
  currency("USD")
  stock(symbol, currency="USD",multiplier=1)
  
 
  portfolio = "turtles"
  initPortf(name=portfolio,symbol, initDate=initDate)
  account = "turtles"
  initAcct(name=account,portfolios="turtles", initDate=initDate, initEq=initEq)
  
  # 该表保存与策略有关的交易事务相关信息
  # 将其存放到portfolio对象  
  strat_tmp <- xts( as.matrix(t(c(0,0,0,0,0))), order.by=as.POSIXct(initDate) )
  colnames(strat_tmp) <- c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')
  ss <- get('strat')
  ss$strat=strat_tmp
  assign('strat',ss)
  
  stockdata=stockdata['201405/']
  # 构建指标
  #x=get(symbol)
  # 入市 (& System 2 exits)
  stockdata$Min20 <- runMin(stockdata[,grep('Low',colnames(stockdata))], 10)
  stockdata$Max20 <- runMax(stockdata[,grep('High',colnames(stockdata))],10)
    
    # 离场
  stockdata$Min10 <- runMin(stockdata[,grep('Low',colnames(x))], 5)
  stockdata$Max10 <- runMax(stockdata[,grep('High',colnames(x))],5)
    
    # 仓位规模参数c('High','Low','Close')
  stockdata$N <- ATR(stockdata[,c(2,3,4)], n=20, maType=EMA, wilder=TRUE)[,'atr']
  #assign(symbol,x)
  
  # 创建交易
  for( i in 57:NROW(stockdata) ){ # 假设所有日期相??? 57:NROW(x)
    CurrentDate=time(stockdata)[i]
    LastDate=time(stockdata)[i-1]
    #print(CurrentDate)
    equity = getEndEq(account, CurrentDate)  
    p=getPortfolio(portfolio)
    p=p$symbols$stockdata$posPL[LastDate]
    equity=equity - as.double(p[,"Pos.Value"])
    if(i == 57)
      equity=initEq
      
#    x=get(symbol)
    ClosePrice = as.numeric(Cl(stockdata[i,]))
      
    Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
    s = tail(strat$strat,1)
      
    Units = as.numeric(s[,'Pos.Units'])
    TxnPrice = as.numeric(s[,'Txn.Price'])
    N = as.numeric(s[,'Txn.N'])
    Stop = as.numeric(s[,'Stop.Price'])
      #  browser()
    UnitSize = as.numeric(trunc((size * equity)/(stockdata[i-1,'N']*100))) # set 100 to reduce risk
    UnitSize = floor(UnitSize / 100) * 100
    if(UnitSize == 0)
    {
      updatePortf(Portfolio = portfolio, Dates = CurrentDate)
      updateAcct(account, Dates = CurrentDate)
      updateEndEq(account, Dates = CurrentDate)
      next
    }
      # 入市（假设以收盘价填入，因此考虑了滑价）
      if( Posn == 0 ) {
        # 初始化多头仓???
        if( as.numeric(Hi(stockdata[i-1,])) > as.numeric(stockdata[i-2,'Max20']) ) {
          addTxn(Portfolio=portfolio, Symbol=symbol,
                 TxnDate=CurrentDate, TxnPrice=ClosePrice,
                 TxnQty = UnitSize , TxnFees=-10, verbose=verbose)
          N = as.numeric(stockdata[i-1,'N'])
          updateStrat(Portfolio=portfolio, Symbol=symbol,
                      TxnDate = CurrentDate, PosUnitsQty = 1,
                      UnitSize = UnitSize, StopPrice = (ClosePrice-2*N),
                      TxnPrice = ClosePrice, TxnN = N)
          # browser()
        } 
      } 
      else
        # 离场和止???
        if( ( Posn > 0 && ( as.numeric(Lo(stockdata[i-1,]))  <  as.numeric(stockdata[i-2,'Min10']) || Lo(stockdata[i-1,])  < Stop ) )) {
          addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=-10, verbose=verbose)
          N = as.numeric(stockdata[i-1,'N'])
          updateStrat(Portfolio = portfolio, Symbol = symbol,
                      TxnDate = CurrentDate, PosUnitsQty = 0, UnitSize = UnitSize,
                      StopPrice = NA, TxnPrice = ClosePrice, TxnN = N)
          #  browser()
        } else
           #加到多头仓位
          if( Posn > 0  && Units < maxUnits && Hi(stockdata[i-1,]) > ( TxnPrice + N * 0.5 ) ) {
            addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate,
                   TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=-10, verbose=verbose)
            N = as.numeric(stockdata[i-1,'N'])
            updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate,
                        PosUnitsQty = Units+1, UnitSize = UnitSize,
                        StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)
          }
    
    # 既然已经更新所有交易，是时候将其在订单薄中作标???
    updatePortf(Portfolio = portfolio, Dates = CurrentDate)
    updateAcct(account, Dates = CurrentDate)
    updateEndEq(account, Dates = CurrentDate)
  } # 结束日期循环
  
  endeq=getEndEq(account,Sys.time())
  print(strat)
  
  history=c(endeq,history)
}



<<<<<<< HEAD
if(require(PerformanceAnalytics))  {
  return = Delt(getAccount(account)$summary$End.Eq)
  dev.new()
  charts.PerformanceSummary(as.zoo(return),main="Turtle Demo Performance")  
  dev.new()
  charts.PerformanceSummary(PortfReturns('turtles'),
                            main='Turtle Demo Instrument Return on Equity',geometric=FALSE)
}
getEndEq(account,Sys.time())
library(PerformanceAnalytics)
rets <- PortfReturns(Account='turtles')
rownames(rets) <- NULL
tab.perf <- table.Arbitrary(rets,
                            metrics=c(
                              "Return.cumulative",
                              "Return.annualized",
                              "SharpeRatio.annualized",
                              "CalmarRatio"),
                            metricsNames=c(
                              "Cumulative Return",
                              "Annualized Return",
                              "Annualized Sharpe Ratio",
                              "Calmar Ratio"))
=======

>>>>>>> ca757e95d9b42af2e38a3f1a20f11cc56df6bcdc
