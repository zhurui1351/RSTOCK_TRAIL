require(quantmod)
require(TTR)
require(blotter)

# 清理R环境，如果该demo程序之前运行过
try(rm("account.turtles","portfolio.turtles",pos=.blotter),silent=TRUE)
try(rm("portfolio","account","N",
       "symbol","symbols","ClosePrice","CurrentDate",
       "equity","Units","maxUnits","size","Stop","equity",
       "TxnPrice","initDate","initEq","Posn","verbose"),silent=TRUE)


# 设定初始值
initDate="2008-01-01"
initEq=100000
print("Initializing portfolio and account structure")
# 构建一个带三只股票的小型组合
symbols = c("XLF", "XLP", "XLE")#, "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}

#创建函数保存过程值
updateStrat <- function(Portfolio, Symbol, TxnDate,
                        PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)
{ # @作者 Peter Carl
  
  # 描述：
  # 添加交易事务相关数据到STRATEGY时间序列
  
  # 输入：
  # TxnDate: 以ISO 8106格式的交易日期，例如：'2008-09-01'
  # PosUnitsQty: 总交易数量（股数）
  # StopPrice: 交易完成价格
  # TxnPrice: 最后交易价格
  # TxnN: 为最后交易结算N
  
  # 输出：
  # 没有输出。在本地命名空间修改STRATEGY
  
  # 函数：
  # 保存交易事务与计算，返回投资组合
  pname=Portfolio
  NewTxn = xts(t(c(PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)), order.by=as.POSIXct(TxnDate))
  colnames(NewTxn) = c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')
  Portfolio<-getPortfolio(Portfolio)
  Portfolio[[Symbol]]$strat <- rbind(Portfolio[[Symbol]]$strat, NewTxn)
  assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
}

getSymbols(symbols, index.class="POSIXct", from=initDate, source="yahoo")
# getSymbols缺省为Date索引。在此我们将其改为用POSIXct。
# getSymbols(symbols, index.class=c("POSIXt","POSIXct"), from=initDate, source="yahoo")

# 创建一个投资组合对象和一个账户对象
portfolio = "turtles"
initPortf(name=portfolio,symbols, initDate=initDate)
account = "turtles"
initAcct(name=account,portfolios="turtles", initDate=initDate, initEq=initEq)

# 该表保存与策略有关的交易事务相关信息
# 将其存放到portfolio对象
Portfolio<-getPortfolio(portfolio)
for(symbol in symbols){
  Portfolio[[symbol]]$strat <- xts( as.matrix(t(c(0,0,0,0,0))), order.by=as.POSIXct(initDate) )
  colnames(Portfolio[[symbol]]$strat) <- c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')
}
# 现在再将其放回所属处
assign( "portfolio.turtles", Portfolio , envir=.blotter )
rm("Portfolio")

# 构建指标
print("Setting up indicators")
for(symbol in symbols){
  # 系统 1
  #
  # 如果最后的突破导致一个获利交易，就忽略那么20-天突破
  #
  # 这些值也用于系统2的离场
  x=get(symbol)
  # 入市 (& System 2 exits)
  x$Min20 <- runMin(x[,grep('Low',colnames(x))], 20)
  x$Max20 <- runMax(x[,grep('High',colnames(x))],20)
  
  # 离场
  x$Min10 <- runMin(x[,grep('Low',colnames(x))], 10)
  x$Max10 <- runMax(x[,grep('High',colnames(x))],10)
  
  # 系统 2
  #
  # 总是取55-天突破
  
  # 入市
  x$Min55 <- runMin(x[,grep('Low',colnames(x))], 55)
  x$Max55 <- runMax(x[,grep('High',colnames(x))],55)
  
  # 仓位规模参数c('High','Low','Close')
  x$N <- ATR(x[,c(2,3,4)], n=20, maType=EMA, wilder=TRUE)[,'atr']
  assign(symbol,x)
}
# 投资组合参数
size = 0.01
maxUnits = 4
Units=0
verbose=TRUE


# 创建交易
for( i in 57:NROW(x) ) { # 假设所有日期相同
  CurrentDate=time(x)[i]
  #print(CurrentDate)
  equity = getEndEq(account, CurrentDate)
  
  for(symbol in symbols){
    x=get(symbol)
    ClosePrice = as.numeric(Cl(x[i,]))
    
    Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
    s = tail(getPortfolio(portfolio)[[symbol]]$strat,1)
    
    Units = as.numeric(s[,'Pos.Units'])
    TxnPrice = as.numeric(s[,'Txn.Price'])
    N = as.numeric(s[,'Txn.N'])
    Stop = as.numeric(s[,'Stop.Price'])
    
    UnitSize = as.numeric(trunc((size * equity)/(x[i-1,'N']*ClosePrice)))
    
    # 入市（假设以收盘价填入，因此考虑了滑价）
    if( Posn == 0 ) {
      # 初始化多头仓位
      if( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max55']) ) {
        addTxn(Portfolio=portfolio, Symbol=symbol,
               TxnDate=CurrentDate, TxnPrice=ClosePrice,
               TxnQty = UnitSize , TxnFees=0, verbose=verbose)
        N = as.numeric(x[i-1,'N'])
        updateStrat(Portfolio=portfolio, Symbol=symbol,
                    TxnDate = CurrentDate, PosUnitsQty = 1,
                    UnitSize = UnitSize, StopPrice = (ClosePrice-2*N),
                    TxnPrice = ClosePrice, TxnN = N)
      } else
        # 初始化空头仓位
        if( as.numeric(Lo(x[i-1,]))  < as.numeric(x[i-2,'Min55']) ) {
          addTxn(Portfolio=portfolio, Symbol=symbol,
                 TxnDate=CurrentDate, TxnPrice=ClosePrice,
                 TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
          N = as.numeric(x[i-1,'N'])
          updateStrat(Portfolio=portfolio, Symbol = symbol,
                      TxnDate = CurrentDate, PosUnitsQty = Units, UnitSize = UnitSize,
                      StopPrice = (ClosePrice +2*N), TxnPrice = ClosePrice, TxnN = N)
        }
    } else
      # 离场和止损
      if( ( Posn > 0 && ( as.numeric(Lo(x[i-1,]))  <  as.numeric(x[i-2,'Min20']) || Lo(x[i-1,])  < Stop ) ) ||
            ( Posn < 0 && ( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max20']) || Hi(x[i-1,]) > Stop ) ) ) {
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
        N = as.numeric(x[i-1,'N'])
        updateStrat(Portfolio = portfolio, Symbol = symbol,
                    TxnDate = CurrentDate, PosUnitsQty = 0, UnitSize = UnitSize,
                    StopPrice = NA, TxnPrice = ClosePrice, TxnN = N)
      } else
        # 加到多头仓位
        if( Posn > 0  && Units < maxUnits && Hi(x[i-1,]) > ( TxnPrice + N * 0.5 ) ) {
          addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
          N = as.numeric(x[i-1,'N'])
          updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate,
                      PosUnitsQty = Units+1, UnitSize = UnitSize,
                      StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)
        } else
          # 加到空头仓位
          if( Posn < 0 && Units < maxUnits && Lo(x[i-1,])  < ( TxnPrice - N * 0.5 ) ) {
            addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate,
                   TxnPrice=Cl(x[i,]), TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
            N = as.numeric(x[i-1,'N'])
            updateStrat(Portfolio=portfolio, Symbol=symbol, TxnDate = CurrentDate,
                        PosUnitsQty = Units+1, UnitSize = UnitSize, StopPrice = (ClosePrice+2*N),
                        TxnPrice = ClosePrice, TxnN = N)
          } #else
    # 维持仓位
  } # 结束证券代码循环
  # 既然已经更新所有交易，是时候将其在订单薄中作标记
  updatePortf(Portfolio = portfolio, Dates = CurrentDate)
  updateAcct(account, Dates = CurrentDate)
  updateEndEq(account, Dates = CurrentDate)
} # 结束日期循环

# 最终的数值
cat('Return: ',(getEndEq(Account=account, Date=CurrentDate)-initEq)/initEq,'\n')

if (require(quantmod)) {
  for(symbol in symbols){
    dev.new()
    chart.Posn(Portfolio='turtles',Symbol=symbol)
  }
}

if(require(PerformanceAnalytics)){
  return = Delt(getAccount(account)$summary$End.Eq)
  dev.new()
  charts.PerformanceSummary(as.zoo(return),main="Turtle Demo Performance")  
  dev.new()
  charts.PerformanceSummary(PortfReturns('turtles'),
                            main='Turtle Demo Instrument Return on Equity',geometric=FALSE)
}

getEndEq(account,Sys.time())