require(quantmod)
require(blotter)

RBreaker <- function(priceData,ratio_up = 0.35,b1 = 0.25,f1 = 0.07,initEq = 1000000,initDate = '2000-01-01',minute=1,verbose=T)
{
  
  daydata = to.daily(priceData)
  colnames(daydata) <- c("Open","High","Low","Close","Volume")
  minute_data <<- to.period(priceData,'minutes',minute)
  
  logger <<- new.env()
  logger$record <- data.frame()
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
    bbreak = ssetup - b1*(ssetup-bsetup)
    sbreak = bsetup + b1*(ssetup-bsetup)
    
    if(strftime(time(m_data[1]),format='%H:%M:%S') == "00:00:00")
    {
      start = 2
    }
    else
    {
      start = 1
    }
    Posn = 0
    # minutes by minutes
    for(i in start:nrow(m_data))
    {
      CurrentDate = time(m_data[i])
      ClosePrice = as.numeric(Cl(m_data[i]))
      UnitSize = 1000
      #enter
      if(Posn == 0 && i!=nrow(m_data))
      {
        if(ClosePrice > bbreak)
        {
          Posn = UnitSize
          logger$record <- rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='enterlong'))
          print('enter long:' )
        }
        if(ClosePrice < sbreak)
        {
           Posn =- UnitSize          
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='entershort'))
          print('enter short:' )
        }
      }
      if(Posn > 0)
      {
        if(i == nrow(m_data))
        {
          Posn = 0
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='exitlong'))          
          print("exit long")
        }
        else
        {
          #今高
          highUnitlNow = max(Hi(m_data[start:i]))#
          if( ClosePrice <scenter && highUnitlNow > ssetup)
          {
            logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='stoplong'))
            Posn = 0
            print('stop long')
            Posn = -UnitSize
           logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='openshort'))
            
            print('open short')        
          }
        }
      }
      if(Posn < 0)
      {
        if(i == nrow(m_data))
        {
          
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='exitshort'))
          Posn = 0
          
          print("exit short")
          
        }
        else
        {
          #今低
          lowUnitlNow = min(Lo(m_data[start:i]))#
          if( ClosePrice > bcenter && lowUnitlNow < bsetup )
          {
            print('stop short')
            logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='stopshort'))
            Posn = 0
            Posn = UnitSize
          logger$record<-rbind(logger$record,data.frame(date=CurrentDate,price=ClosePrice,type='openlong'))            
            print('open long')
          }
        }
        
      }
    }
  }
}
