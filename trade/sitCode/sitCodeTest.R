require(quantmod)
con = gzcon(file('c:/investor/sit.gz', 'rb'))
source(con)
close(con)
tickers = spl('EURUSD,USDJPY,GBPUSD,AUDUSD,USDCHF,USDCAD')
data <- new.env()   

if(file.exists('tickers.RData'))
{
  load('tickers.RData',env=data)
  print('ok')
} else
{
  getSymbols.fxhistoricaldata(tickers, 'hour', data, download=T) 
  save(list=ls(data),envir=data,file='tickers.RData')
}
 
bt.prep(data, align='remove.na', dates='1990::')

prices = data$prices   
n = len(tickers)  
models = list()

# Equal Weight
data$weight[] = NA
data$weight[] = ntop(prices, n)
models$equal.weight = bt.run.share(data, clean.signal=F)

# Timing by M. Faber
sma = bt.apply.matrix(prices, SMA, 200)
data$weight[] = NA
data$weight[] = ntop(prices, n) * (prices > sma)
models$timing = bt.run.share(data, clean.signal=F)

# Report
models = rev(models)
plotbt.custom.report.part1(models)
plotbt.custom.report.part2(models)  
