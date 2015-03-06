require(quantmod)
require(blotter)
path = "D:/stock/FOREX"
reportpath = "D:/myreport_m"
f = 'SH000001_5ms.TXT'
fname = file.path(path,f)

priceData <- read.table(fname,sep='\t',header=T)
priceData=xts(priceData[2:7],order.by=as.POSIXct(priceData[,1]))
colnames(priceData) <- c("Open","High","Low","Close","Volume","Amount")

.blotter <- new.env()
symbol = "minute_data"
currency("USD")
stock(symbol, currency="USD",multiplier=1)

initEq = 1000000
initDate = '2000-01-01'

initPortf(name=symbol,symbol, initDate=initDate)
initAcct(name=symbol,portfolios=symbol, initDate=initDate, initEq=initEq)
verbose=T

daydata = to.daily(priceData)
colnames(daydata) <- c("Open","High","Low","Close","Volume")

all_days = unique(strftime(index(priceData),'%Y-%m-%d'))
for(day in all_days)
{
  intradaydata = priceData[day]
  minute_data = to.period(intradaydata,'minutes',30) 
  temp_high = as.numeric(Hi(minute_data[1]))
  temp_low = as.numeric(Lo(minute_data[1]))
  #test in one day begin at 2th data in data series
  for(i in 2:nrow(minute_data))
  {  
    CurrentDate=time(minute_data)[i]
    equity<-getEndEq(symbol, CurrentDate)
    Posn <- getPosQty(symbol, Symbol=symbol, Date=CurrentDate)
    #0.1 every time 
    ClosePrice <- as.numeric(Cl(minute_data[i]))
    UnitSize <-as.numeric(trunc(equity * 0.1 /ClosePrice))
    #enter
    if(Posn == 0)
    {
      if(ClosePrice > temp_high)
      {
        addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0,verbose=verbose)
        print('enter :' )
      }
    }
    else
    {
      #exit when close if position not equal to 0
      if(i == nrow(minute_data))
      {
        addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
        print("exit")
      }
      #stop
      if(ClosePrice < temp_low)
      {
        addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
        print('stop')
      }
      
    }
    updatePortf(symbol, Dates = CurrentDate)
    updateAcct(symbol, Dates = CurrentDate)
    updateEndEq(symbol, Dates = CurrentDate)
  }
  
}
