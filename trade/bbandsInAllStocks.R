require(quantmod)
require(TTR)
library(blotter)
library(PerformanceAnalytics)
#历史记录表
history<-c()
#股票文件所在目录，以txt形式存储
path="D:/stock/dest"
Sys.setenv(TZ="UTC")
files<-dir(path)
#调试单个股票用
#files=c('SH600100.TXT')
#遍历所有股票
for(f in files)
{
  print(f)
  #清除环境变量
  try(rm("portfolio","account"),silent=TRUE)
  try(rm("account.stockdata","portfolio.stockdata",pos=.blotter),silent=TRUE)
  fname<-file.path(path,f)
  #读入股票数据
  try(stockdata<-read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
  colnames(stockdata)<-c("Open","High","Low","Close","Volume","Amount")
  #设置时间格式
  time(stockdata)=as.POSIXct(time(stockdata))
  #转化为xts对象
  stockdata=as.xts(stockdata)
  #测试2010年后的数据
  stockdata=stockdata['2010/']
  #求布林通道信息
  bbands=BBands(Cl(stockdata))
  stockdata=cbind(stockdata,bbands)
  
  #初始化账户、头寸等信息
  symbol = c("stockdata")
  currency("RMB")
  stock(symbol, currency="RMB",multiplier=1)
  
  initDate="2000-01-01"
  initEq=1000000
  portfolio = "stockdata"
  initPortf(name=portfolio,symbol, initDate=initDate)
  account = "stockdata"
  initAcct(name=account,portfolios=portfolio, initDate=initDate, initEq=initEq)
  #不用打印成交信息
  verbose=FALSE
  #遍历每只股票的历史
  for(i in 2:nrow(stockdata))
  {
    #存在布林值时开始计算
    if(!is.na(stockdata[i,'dn']))
    {
      CurrentDate=time(stockdata)[i]
      equity<-getEndEq(portfolio, CurrentDate)
      Posn <- getPosQty(portfolio, Symbol=symbol, Date=CurrentDate)
      #无资金管理,每次满仓
      UnitSize <-as.numeric(trunc(equity/ClosePrice))
      ClosePrice <- as.numeric(Cl(stockdata[i,]))
      
      #持仓为零时，判断是否买入
      if(Posn ==  0)
      {
        #收盘价小于下限，买入
        if(ClosePrice < stockdata[i,'dn'])
        {
          addTxn(portfolio, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=-1 * ClosePrice * UnitSize * 0.01,verbose=verbose) 
        }
      }
      else
      {
        #收盘价大于上限，卖出
        if(ClosePrice > stockdata[i,'up'])
        {
          addTxn(portfolio, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=-1 * ClosePrice * UnitSize * 0.01,verbose=verbose) 
        }
      }
      #更新账户信息
      updatePortf(portfolio, Dates = CurrentDate)
      updateAcct(portfolio, Dates = CurrentDate)
      updateEndEq(portfolio, Dates = CurrentDate)
    }
  }
  #计算简单盈亏率
  endeq=getEndEq(account,Sys.time())
  ratio=(endeq-initEq)/initEq
  result=c(ratio)
  names(result)=f
  #保存到hisory中
  history=c(result,history)
}