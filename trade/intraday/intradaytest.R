require(quantmod)
con = gzcon(file('c:/investor/sit.gz', 'rb'))
source(con)
close(con)

load.packages('quantmod')
New_MA <- function (){
  tickers = spl('GBPUSD')
  
  capital = 10000
  data <- new.env()
  if(file.exists('c:/R/GBPUSD_HOUR.Rdata'))
  {
    load('c:/R/GBPUSD_HOUR.Rdata')
  }
  else
  {
   
   getSymbols.fxhistoricaldata(tickers, 'hour', data, download=TRUE)
   save(data,file='c:/R/GBPUSD_HOUR.Rdata')
  }
  #  bt.prep function merges and aligns all symbols in the data environment
  bt.prep(data, align='remove.na', dates='2009::2010')
  
  prices = data$prices
  # n = len(tickers)
  models = list()
  
  
  #*****************************************************************
  # Code Strategies
  #******************************************************************
  data$weight[] = NA
  data$weight[] = 1
  models$buy.hold = bt.run.share(data, clean.signal=TRUE
                                 #,capital=capital
  )
  #*****************************************************************
  # Code Strategies : SMA Fast/Slow cross over
  #******************************************************************
  sma.fast = SMA(prices, 20)
  sma.slow = SMA(prices, 50)
  
  # data$weight matrix holds weights (signals) to open/close positions.
  data$weight[] = NA
  data$weight[] = iif(cross.up(sma.fast, sma.slow), 1, iif(cross.dn(sma.fast, sma.slow), -1, NA))
  
  # bt.run computes the equity curve of strategy specified by data$weight matrix
  models$ma.cross = bt.run.share(data, clean.signal=TRUE, trade.summary = TRUE
                                 , do.lag = 2
                                 #, commission=0.0002
                                 #, capital=capital
  )
  #*****************************************************************
  # Code Strategies : MA Cross Over
  #******************************************************************
     sma = bt.apply(data, function(x) { SMA(Cl(x), 200) } )
     data$weight[] = NA
     data$weight[] = iif(prices >= sma, 1, 0)
     sma.cross = bt.run(data, trade.summary=T)
  
  #*****************************************************************
  # Create Report
  #******************************************************************
  
  # plotbt.custom.report function creates the customized report, which can be fined tuned by the user
  plotbt.custom.report(models$ma.cross, models$buy.hold)
  plotbt.custom.report(models$ma.cross, models$buy.hold, trade.summary=TRUE)
  strategy.performance.snapshoot(models, T)
  models$ma.cross$trade.summary
}

testdata =data$GBPUSDhour_
#取日数据
all_days = unique(strftime(index(testdata),'%Y-%m-%d'))
for(day in all_days[1:2])
{
 daydata = testdata[day]
 h_data = to.period(daydata,'hours',3)
}
