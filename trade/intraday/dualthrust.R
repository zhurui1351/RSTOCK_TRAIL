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
  #initAcct(name=symbaol,portfolios=symbol, initDate=initDate, initEq=initEq)
  
  #日数
  daydata = to.daily(priceData)
  colnames(daydata) <- c("Open","High","Low","Close","Volume")
  
  daydata$HH = runMax(Hi(daydata),n)
  daydata$HC = runMax(Cl(daydata),n)
  daydata$LC = runMin(Cl(daydata),n)
  daydata$LL = runMin(Lo(daydata),n)
  minute_data = to.period(priceData,'minutes',minute)
  minute_data$sma = SMA(Cl(minute_data),5)
  m_data <<-c()
  daydata$atr = ATR(daydata,n=3)$atr
  logger <<- new.env()
  logger$record <- data.frame()
  #day by day
  Posn = 0
  bar = 0
  for(day in (n+1):nrow(daydata))
  {
    #截止昨天，n日内的range
    range = max(daydata[day-1]$HH-daydata[day-1]$LC,daydata[day-1]$HC-daydata[day-1]$LL)
    
    date = as.character(time(daydata[day]))
    m_data <<- minute_data[date]
    if(nrow(m_data) < 2)
    {
      next
    }  
   # range = as.numeric(daydata[day-1]$atr)
    if(is.na(range)) {next}
    upper_line = as.numeric(Op(m_data[1])) + ks * range
    lower_line = as.numeric(Op(m_data[1])) - kx * range
    
    stoprange =0.5*(upper_line - lower_line)
    stopprice = as.numeric(Op(m_data[1])) + stoprange
    upstopprice = upper_line - 0.2 *range
    lowstopprice = lower_line + 0.2 *range
  
    print(date)
  #  print(upper_line)
   # print(lower_line)
    
     firstdate=time(m_data[1])
     myindex =which( index(minute_data)== firstdate)
    if(strftime(time(m_data[1]),format='%H:%M:%S') == "00:00:00")
    {
      start = 2
    #  myindex = myindex + 1
    }
    else
    {
      start = 1
    }
    #每日开头，头寸为0 
   #  Posn = 0
    # minutes by minutes
    for(i in start:nrow(m_data))
    {
      CurrentDate=time(m_data[i])
      if(Posn != 0){
        bar = bar + 1
      }
      
     # from = myindex-4
    #  to = myindex-1
     # myindex = myindex + 1
    #  consec = diff( Cl(minute_data[from:to]))[2:4]
    #  isallup = all(consec > 0)
     # isalldown = all(consec < 0)
     # from = myindex-4
    #   to = myindex-1
     # myindex = myindex + 1
      # consec = diff( minute_data$sma[from:to])[2:4]
      # isallup = all(consec > 0)
      #isalldown = all(consec < 0)
    #  y = as.numeric(minute_data$sma[from : to])
     # x = seq(0.01,0.04,by = 0.01)
  #  x = 0.4*c(1:4)
    
   # f = lm(y~x )
    #coff =as.numeric(f$coefficients[2])
    #angle = atan(coff) / pi * 180
    
  #  print(angle)
      #0.1 every time 
      ClosePrice <- as.numeric(Cl(m_data[i]))
      UnitSize <- 1000 
      
      #enter,the last bar dont enter
      if(Posn == 0)# && i!=nrow(m_data) )
      {
        #long
        if(ClosePrice > upper_line)#  && angle> 8)#isallup) # )
        {
          
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='enterlong'))          
          Posn = UnitSize
          upstopprice = upper_line - 0.5 *range
          price = ClosePrice
   #       print('enter long:' )
        }
        #short
        if(ClosePrice < lower_line )#&& angle< -8)#isalldown)#)# 
        {
         
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='entershort'))
          Posn = -UnitSize
          lowstopprice = lower_line + 0.5 *range
          price = ClosePrice
    #      print('enter short:' )
        }
      }
      if(Posn >0)
      {
        
        #exit when close if position not equal to 0
        if(i == nrow(m_data)) #bar == 3)#
        {
      #     logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='exitlong'))
       #    Posn = 0
      #     bar = 0
       #   print("exit long")
        }
     #   else
        {          
          #stop and short
          if(ClosePrice < upstopprice)#lower_line) # stopprice)#  
          {
            logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='stoplong'))
            Posn = 0
          #  bar = 0
      #      print('stop long')
      #      logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='openshort'))
       #     Posn = -UnitSize           
           # print('open short')
            
          }else{
            if((ClosePrice - price) > 0.1*range)
            {
              price = ClosePrice
              upstopprice =  upstopprice + 0.1*range #ClosePrice#
              
            }
            
          }
      
        }
      }
      if(Posn < 0)
      {
        #exit when close if position not equal to 0
        if(i == nrow(m_data)) #)(bar == 3)#
        {
          
    #      logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='exitshort'))
   #       Posn = 0
     #     bar = 0
       #   print("exit short")
          
        }
     #   else
        {
          #stop
          if(ClosePrice >  lowstopprice)#upper_line)# stopprice)# 
          {
            logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='stopshort'))
            Posn = 0
      #      bar = 0
        #    print('stop short')
      #      logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='openlong'))
       #     Posn = UnitSize  
         #   print('open long')
          }else{
            if((ClosePrice - price) < -0.1*range)
            {
              price = ClosePrice
              lowstopprice =  lowstopprice - 0.1*range#ClosePrice#
            }
          }
        }
      }
    }    
  }  
}
