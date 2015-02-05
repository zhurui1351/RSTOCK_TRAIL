library(quantmod)
library(TTR)
library(quantstrat)
path="D:/stock/dest"
files<-dir(path)
f = files[100]
Sys.setenv(TZ="UTC")
fname<-file.path(path,f)
try(stockdata<-read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
colnames(stockdata)<-c("Open","High","Low","Close","Volume","Amount")
time(stockdata)=as.POSIXct(time(stockdata))
stockdata=as.xts(stockdata)
chartSeries(stockdata)
#chartSeries(stockdata,TA = "addVo(); addSMA(); addEnvelope();addMACD(); addROC()")
chartSeries(stockdata)
#plot(add_SMA(n=10,with.col=4, on=1))
plot(add_SMA(n=20,col=4, on=1))
#initDate="2000-01-01"
initEq=10000
print("Initializing portfolio and account structure")
# 构建一个带三只股票的小型组合
symbols = c("stockdata")
currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}
portfolio = "2ma"
rm.strat(portfolio)
initPortf(name=portfolio,symbols, initDate=initDate)
account = "2ma"
initAcct(name=account,portfolios="2ma", initDate=initDate, initEq=initEq)
stockdata$firstma<-SMA(Cl(stockdata),5)
stockdata$secondma<-SMA(Cl(stockdata),20)
stockdata=stockdata['2010/']
# 构建指标
x=stockdata
print("Setting up indicators")
for( i in 2:NROW(x) ){
  CurrentDate <- time(stockdata)[i]
  equity = getEndEq(portfolio, CurrentDate)
  ClosePrice <- as.numeric(Cl(stockdata[i,]))
  Posn <- getPosQty(portfolio, Symbol=symbol, Date=CurrentDate)
  UnitSize = as.numeric(trunc(equity/ClosePrice))
  if( as.numeric(x[i-1,'firstma']) < as.numeric(x[i-1,'secondma']) && as.numeric(x[i,'firstma']) > as.numeric(x[i,'secondma']) )
  {
    addTxn(portfolio,Symbol=symbol,TxnDate=CurrentDate,
           TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0)
  }else{
    if(as.numeric(x[i-1,'firstma']) > as.numeric(x[i-1,'secondma']) && as.numeric(x[i,'firstma']) < as.numeric(x[i,'secondma']))
    {
      addTxn(portfolio, Symbol=symbol, TxnDate=CurrentDate,
             TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0)
    }}
  updatePortf(portfolio, Dates = CurrentDate) 
  updateAcct(portfolio, Dates = CurrentDate)
  updateEndEq(portfolio, Dates = CurrentDate)
}
cat('Return: ',(getEndEq(Account=account, Date=CurrentDate)-initEq)/initEq,'\n')
tstats <- tradeStats(Portfolio=portfolio)
pertstat<-perTradeStats(portfolio)
