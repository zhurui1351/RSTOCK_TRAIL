require(quantmod)
require(blotter)

dualthrust <- function(priceData,n=3,ks = 0.35,kx = 0.2,initEq = 1000000,minute=1,initDate = '2000-01-01',verbose=F)
{
  #init portofolio
 # .blotter <<- new.env()
#  symbol = "m_data"
 # currency("USD")
  #stock(symbol, currency="USD",multiplier=1)
  
  #initPortf(name=symbol,symbol, initDate=initDate)
  #initAcct(name=symbol,portfolios=symbol, initDate=initDate, initEq=initEq)
  
  #日数
  daydata = to.daily(priceData)
  colnames(daydata) <- c("Open","High","Low","Close","Volume")
  
  daydata$HH = runMax(Hi(daydata),n)
  daydata$HC = runMax(Cl(daydata),n)
  daydata$LC = runMin(Cl(daydata),n)
  daydata$LL = runMin(Lo(daydata),n)
  minute_data = to.period(priceData,'minutes',minute)
  m_data <<-c()
  daydata$atr = ATR(daydata,n=n)$atr
  logger <<- new.env()
  logger$record <- data.frame()
  #day by day
  for(day in (n+1):nrow(daydata))
  {
    #截止昨天，n日内的range
   # range = max(daydata[day-1]$HH-daydata[day-1]$LC,daydata[day-1]$HC-daydata[day-1]$LL)
    range = as.numeric(daydata[day-1]$atr)
    if(is.na(range)) {next}
    date = as.character(time(daydata[day]))
    m_data <<- minute_data[date]
    if(nrow(m_data) < 2)
    {
      next
    }
    upper_line = as.numeric(Op(m_data[1])) + ks * range
    lower_line = as.numeric(Op(m_data[1])) - kx * range
    stoprange =0.5*(upper_line - lower_line)
    stopprice = as.numeric(Op(m_data[1])) + stoprange
    print(date)
    print(upper_line)
    print(lower_line)
    if(strftime(time(m_data[1]),format='%H:%M:%S') == "00:00:00")
    {
      start = 2
    }
    else
    {
      start = 1
    }
    #每日开头，头寸为0 
     Posn = 0
    # minutes by minutes
    for(i in start:nrow(m_data))
    {
      CurrentDate=time(m_data[i])
     # equity<-getEndEq(symbol, CurrentDate)
      # if(equity == 0) stop('equit is empty')
      #Posn <- getPosQty(symbol, Symbol=symbol, Date=CurrentDate)
      #0.1 every time 
      ClosePrice <- as.numeric(Cl(m_data[i]))
      UnitSize <- 1000 
      
      #enter,the last bar dont enter
      if(Posn == 0 && i!=nrow(m_data))
      {
        #long
        if(ClosePrice > upper_line)
        {
          
          #addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
         #        TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0,verbose=verbose)
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='enterlong'))          
          Posn = UnitSize
          print('enter long:' )
        }
        #short
        if(ClosePrice < lower_line)
        {
         
         # addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
        #         TxnPrice=ClosePrice, TxnQty = -UnitSize , TxnFees=0,verbose=verbose)
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='entershort'))
          Posn = -UnitSize
          print('enter short:' )
        }
      }
      if(Posn >0)
      {
        #exit when close if position not equal to 0
        if(i == nrow(m_data))
        {
          #addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
          #       TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='exitlong'))
          Posn = 0
          print("exit long")
        }
        else
        {
          
          #stop and short
          if(ClosePrice < lower_line) # stopprice)#
          {
           # addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
           #        TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
            logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='stoplong'))
            Posn = 0
            print('stop long')
           # addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
            #       TxnPrice=ClosePrice, TxnQty = -UnitSize , TxnFees=0,verbose=verbose)
          #  logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='openshort'))
          #  Posn = -UnitSize
           
           # print('open short')
            
          }
        }
      }
      if(Posn < 0)
      {
        #exit when close if position not equal to 0
        if(i == nrow(m_data))
        {
          
         # addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
         #        TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='exitshort'))
          Posn = 0
          print("exit short")
          
        }
        else
        {
          #stop
          if(ClosePrice >  upper_line)# stopprice)#
          {
         #   addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
          #         TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0,verbose=verbose) 
            logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='stopshort'))
            Posn = 0
            print('stop short')
          #  addTxn(symbol, Symbol=symbol, TxnDate=CurrentDate,
         #          TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0,verbose=verbose)
          #  logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='openlong'))
           # Posn = UnitSize  
         #   print('open long')
          }
        }
      }
    #  updatePortf(symbol, Dates = CurrentDate)
    #  updateAcct(symbol, Dates = CurrentDate)
    #  updateEndEq(symbol, Dates =CurrentDate)
    }
    
  }
  
}


