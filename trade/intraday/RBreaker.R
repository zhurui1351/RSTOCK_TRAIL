require(quantmod)
require(blotter)

RBreaker <- function(priceData,ratio_up = 0.35,b1 = 0.25,f1 = 0.07,initEq = 1000000,initDate = '2000-01-01',minute=1,verbose=T)
{
  .blotter <<- new.env()
  symbol = "m_data"
  currency("USD")
  stock(symbol, currency="USD",multiplier=1)
  
  initPortf(name=symbol,symbol, initDate=initDate)
  initAcct(name=symbol,portfolios=symbol, initDate=initDate, initEq=initEq)
  
  daydata = to.daily(priceData)
  colnames(daydata) <- c("Open","High","Low","Close","Volume")
  minute_data <<- to.period(priceData,'minutes',minute)
  # assign('minute_data',to.period(priceData,'minutes',minute), parent.env(environment()))
  #要利用前一天的数据，因此从2开始
  for(day in 2:nrow(daydata))
  {
    date = as.character(time(daydata[day]))
    print(date)
    m_data <<- minute_data[date]
    if(nrow(m_data) < 2)
    {
      next
    }
    
    lastHigh = as.numeric(Hi(daydata[day-1]))
    lastLow = as.numeric(Lo(daydata[day-1]))
    lastClose = as.numeric(Cl(daydata[day-1]))
    lastOpen = as.numeric(Op(daydata[day-1]))
    
    ssetup = lastHigh + ratio_up * (lastClose - lastLow)
    scenter = ((1+f1) / 2)  * (lastHigh + lastLow) - f1*lastLow
    bcenter = ((1+f1) / 2)  * (lastHigh + lastLow) - f1*lastHigh
    
    bsetup = lastLow - ratio_up * (lastHigh - lastClose)
    bbreak = ssetup + b1*(ssetup-bsetup)
    sbreak = bsetup - b1*(ssetup-bsetup)
    
    if(strftime(time(m_data[1]),format='%H:%M:%S') == "00:00:00")
    {
      start = 2
    }
    else
    {
      start = 1
    }
    # minutes by minutes
    for(i in start:nrow(m_data))
    {
      CurrentDate = time(m_data[i])
      equity = getEndEq(symbol, CurrentDate)
      if(equity == 0) stop('equit is empty')
      Posn = getPosQty(symbol, Symbol=symbol, Date=CurrentDate)
      #0.1 every time 
      ClosePrice = as.numeric(Cl(m_data[i]))
      UnitSize = 1000
      #enter
      if(Posn == 0)
      {
        if(ClosePrice > bbreak)
        {
          addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0,verbose=verbose)
          print('enter long:' )
        }
        if(ClosePrice < sbreak)
        {
          addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = -UnitSize , TxnFees=0,verbose=verbose)
          print('enter short:' )
        }
      }
      if(Posn > 0)
      {
        if(i == nrow(m_data))
        {
          addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
          print("exit long")
        }
        else
        {
          #今高
          highUnitlNow = max(Hi(m_data[start:i]))
          if(highUnitlNow > ssetup && ClosePrice <scenter)
          {
            addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
                   TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
            print('stop long')
            addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
                   TxnPrice=ClosePrice, TxnQty = -UnitSize , TxnFees=0,verbose=verbose)
            print('open short')
          }
        }
      }
      if(Posn < 0)
      {
        if(i == nrow(m_data))
        {
          
          addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
          print("exit short")
          
        }
        else
        {
          #今低
          lowUnitlNow = max(Lo(m_data[start:i]))
          if(lowUnitlNow < bsetup && ClosePrice > bcenter)
          {
            addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
                   TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
            print('stop short')
            addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
                   TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0,verbose=verbose)
            print('open long')
          }
        }
        
      }
      updatePortf(symbol, Dates = CurrentDate)
      updateAcct(symbol, Dates = CurrentDate)
      updateEndEq(symbol, Dates =CurrentDate)
    }
  }
}
