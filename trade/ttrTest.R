library('quantmod')
library('blotter')
#清除环境变量
rm("IBM","strategy","account","initEq","portfolio")
rm("portfolio.IBM","account.IBM",pos=.blotter)
#获取IBM数据
IBM <- getSymbols("IBM",auto.assign=FALSE,from='2010-01-01')
#除权除息
IBM=adjustOHLC(IBM)
#初始化货币
currency("USD")
#初始化证券
stock("IBM", currency = "USD", multiplier = 1)
#设立时区
Sys.setenv(TZ = "UTC") 
#绘图
chartSeries(IBM,subset = "2012/2014",theme = "white",TA = "addSMA(n=5,col='red');addSMA(n=30,col='blue')")
#初始化组合和账户
strategy <- "IBM"
initPortf(strategy, "IBM", initDate = "2010-01-01")
initAcct(strategy, portfolios = strategy, initDate = "2010-01-01", initEq = 1e+06) #初始的资金是1e6，即1，000，000

#计算10日、30日均线
IBM$sma_long <- SMA(Cl(IBM),30)
IBM$sma_short <- SMA(Cl(IBM),5)

print(head(IBM))

#逐个数据处理
for( i in 2:nrow(IBM) )
{
  #获取当前交易日期
  CurrentDate <- time(IBM)[i]
  #获取最近的头寸
  equity<-getEndEq(strategy, CurrentDate)
  #当前仓位
  Posn <- getPosQty(strategy, Symbol='IBM', Date=CurrentDate)
  #全仓交易
  UnitSize <-as.numeric(trunc(equity/ClosePrice))
  #获取当前收盘价
  ClosePrice <- as.numeric(Cl(IBM[i,]))
  #获取当前长均线值
  sma_long_current <- as.numeric(IBM[i,'sma_long'])
  #获取当前短均线值
  sma_short_current <- as.numeric(IBM[i,'sma_short'])
  
  #获取上一期长均线值
  sma_long_pre <- as.numeric(IBM[i-1,'sma_long'])
  #获取上一期短均线值
  sma_short_pre <- as.numeric(IBM[i-1,'sma_short'])
  #均线开始有值
  if(!is.na(sma_long_pre))
  {
    #如果没有头寸
    if(Posn ==  0)
    {
      #当短期均线向上穿越长线时(前期短均线小于长均线，当期短均线大于等于长均线)，买入
      if((sma_short_pre < sma_long_pre ) && (sma_short_current >= sma_long_current))
      {
        addTxn(strategy, Symbol='IBM', TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=-1 * ClosePrice * UnitSize * 0.01) 
      }
    }
    else #持有头寸，判断是否退出
    {
      if((sma_short_pre > sma_long_pre ) && (sma_short_current <= sma_long_current))
      {
        #当短期均线向下穿越长线时(前期短均线大于长均线，当期短均线小于等于长均线)，卖出
        addTxn(strategy, Symbol='IBM', TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty =-Posn , TxnFees=-1 * Posn * 0.01) 
      }
    }
    #更新账户、头寸信息
    updatePortf(strategy, Dates = CurrentDate)
    updateAcct(strategy, Dates = CurrentDate)
    updateEndEq(strategy, Dates = CurrentDate)
  }
}
#获取最终资金额
endeq=getEndEq("IBM",Sys.time())
#绘制资金曲线图
chart.Posn(Portfolio='IBM',Symbol='IBM')
#获得总的交易信息，如笔数、胜负率等
states<-tradeStats("IBM","IBM")
print(states)
#获取每笔交易的信息
perstates<-perTradeStats('IBM','IBM')
print(perstates)
  