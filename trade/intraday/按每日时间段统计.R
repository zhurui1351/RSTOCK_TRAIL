require(quantmod)
require(blotter)
#tzone(priceData) = 'Etc/GMT-8'
#time(priceData) = as.POSIXct(strftime(time(priceData)),tz='Etc/GMT-8')

#from = "20:20:00"
#to = "23:30:00"
#isCrossday = F
timeperiod_asia = list(from="08:00:00",to="14:30:00",iscrossday=F,tag="asia")
timeperiod_eur = list(from="15:30:00",to="23:30:00",iscrossday=F,tag="eur")
timeperiod_us = list(from="20:20:00",to="03:30:00",iscrossday=T,tag="us")
timeperiod_us_eur_cross = list(from="20:20:00",to="23:30:00",iscrossday=F,tag="us_eur_cross")
timeperiod = list(timeperiod_asia,timeperiod_eur,timeperiod_us,timeperiod_us_eur_cross)

getMeanRangeDuraingTime <- function(priceData,from = "20:20:00",to = "23:30:00",isCrossday = F)
{
  pricedata = to.minutes30(priceData)
  daydata = to.daily(priceData)
  daydate = time(daydata)
  v = c()
  iternums = nrow(daydata)
  for(day in 1:iternums)
  {
    if(isCrossday == F)
    {
      date = as.character(daydate[day])
      timefrom = paste(date,from)
      timeto = paste(date,to)
      timeperiod = paste(timefrom,timeto,sep="/")
    }
    else #跨日
    {
      if(day+1 >iternums) next;
      date = as.character(daydate[day])
      date1 = as.character(daydate[day+1])
      timefrom = paste(date,from)
      timeto = paste(date1,to)
      timeperiod = paste(timefrom,timeto,sep="/")
    }
    m_data = pricedata[timeperiod]
    if(nrow(m_data) == 0) 
    {
      #print(date)
      next
    }
    colnames(m_data) <- c("Open","High","Low","Close","volume")
    votality = max(Hi(m_data)) - min(Lo(m_data))
    v = c(v,votality)  
  }
  return(mean(v))
}

getTheSmallestPoint <- function(symbol)
{
  symbols = c(USDJPY=0.01,AUDJPY=0.01,EURUSD=0.0001,AUDUSD=0.0001,CHFJPY=0.01,EURGBP=0.0001,GBPUSD=0.0001,
              NZDUSD=0.0001,USDCAD=0.0001,USDCHF=0.0001,XAGUSD=0.01)
  return(as.numeric(symbols[symbol]))
}

path = "D:/minutedata"
files <- dir(path)
#files = 'USDJPY.txt'
#f = files[1]
meanrange_report = list()
i = 1

for(f in files)
{
  fname = file.path(path,f)
  priceData <- read.table(fname,sep=',',header=T,colClasses = rep(c("NULL", "character", "numeric"), c(1, 2, 5)))  
  priceData <- read.zoo(priceData, sep=",", header=TRUE, 
                        index.column=1:2, format="%Y%m%d %H%M%S", tz="GMT")
  priceData <- as.xts(priceData)  
  colnames(priceData) <- c("Open","High","Low","Close","Volume")
  
  #调到北京时间 
  tzone(priceData) = 'Etc/GMT-8'
  
  symbolname = strsplit(f,'.',fixed=T)
  symbolname = symbolname[[1]][1]
  truepoint = getTheSmallestPoint(symbolname)
  
  for(tp in 1:length(timeperiod))
  {
    from = timeperiod[[tp]]$from
    to = timeperiod[[tp]]$to
    isCrossday = timeperiod[[tp]]$iscrossday
    tag = timeperiod[[tp]]$tag
    mv = getMeanRangeDuraingTime(priceData,from,to,isCrossday)
    truemv = mv / truepoint 
    r1 = paste(symbolname," ", tag," from:",from,"  to:",to," mean range:",truemv)
    meanrange_report[[i]] = r1
    i = i+1 
  }
}
