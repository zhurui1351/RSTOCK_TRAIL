#大周期
#大小周期映射 seq = findInterval(sp,lp+1,rightmost.closed = T) + 1 lp+1 增加1秒，避免左等号
#考虑5分钟 ，参考h均线 h上升，5m背离，做多m个bar

rangeAfterNbarforMbarFilter <- function(pricedata,up=T,condition = 3,m=1,filter=function(...){return(T)})
{
  s = as.numeric(Cl(pricedata) - Op(pricedata)) 
  op = as.numeric(Op(pricedata)) 
  cl = as.numeric(Cl(pricedata))  
  pre = F
  count = 0
  point_list = c(0)
  count_condition_happen = 0
  i=1
  while(i<length(s))
  {
    if(up==T && pre ==F && s[i] >0)
    {
      pre = T         
    }
    if(up==F && pre ==F && s[i] < 0)
    {
      pre = T
    }
    
    if(up==T && pre ==T && s[i] >0)
    {
      
      count =  count + 1
      #return if do not have enough data
      if(i + m > length(s))
      {
        return(list(point=point_list,count=count_condition_happen))
      }
      if(count==condition && filter(index=i))
      {
        count_condition_happen = count_condition_happen + 1
        point = 0
        point_list[count_condition_happen] = cl[i+m] - op[i]
        i = i + m + 1
        pre = F
        count = 0
      }
      else
      {
        i = i + 1
      }
      
    }
    else if(up==F && pre ==T && s[i] < 0)
    {
      count =  count + 1
      #return if do not have enough data
      if(i + m > length(s))
      {
        return(list(point=point_list,count=count_condition_happen))
      }
      if(count==condition && filter(index=i) )
      {
        count_condition_happen = count_condition_happen + 1
        point = 0
        point_list[count_condition_happen] = cl[i+m] - op[i]
        i = i + m + 1
        pre = F
        count = 0      
      }
      else
      {
        i = i + 1
      }   
    }
    else  #the next m day
    {
      pre = F
      count = 0
      i = i +1
    }
  }
  return(list(point=point_list,count=count_condition_happen))
}
filterMaParentFrame <- function(pricedata,longpricedata,shortmaptolong)
{
 
  short = pricedata
  long = longpricedata
  long$sma = SMA(Cl(long),3)
  #小周期映射到大周期
 # shortmaptolong = findInterval(shortp,longp+1,rightmost.closed = T) + 1
  filterMa = function(...)
  {
    para = list(...)
    if(is.null(para$index))
    {
      stop("error param does not have index or pricedata")
    }
    pos = para$index
    longpos = shortmaptolong[pos] 
    if(longpos - 2 <= 1)
    {
      return(F)
    }
    #过去n个大周期bar的sma情况
    smaseq = as.numeric(long$sma[(longpos-2) : longpos])
    xaxel= 1 : 3
    #回归看斜率
    f = lm(smaseq~xaxel)
    slope = f$coefficients[2]
    if(slope < 0)
    {
      return(T)
    }
    else
    {
      return(F)
    }
  }  
  return(filterMa)
}

getLongPeriodDataAndMap <- function(i,pricedata)
{
  map = list("5"=60,"15"=60)
  p = map[[as.character(i)]]
  if(is.null(p))
  {
    short = pricedata
    long = to.daily(pricedata)
    shortp = index(short)
    longp = index(long)
    #小周期映射到大周期
    day = as.Date(strftime(shortp,format='%Y-%m-%d'))
    shortmaptolong = findInterval(day,longp,rightmost.closed = T) 
    return(list(long,shortmaptolong))
  }
  else
  {
    short = pricedata
    long = to.period(pricedata,'minutes',as.numeric(p))
    shortp = index(short)
    longp = index(long)
    #小周期映射到大周期
    shortmaptolong = findInterval(shortp,longp+1,rightmost.closed = T) + 1
    return(list(long,shortmaptolong))
  }
  
}

getTheSmallestPoint <- function(symbol)
{
  symbols = c(USDJPY=0.01,AUDJPY=0.01,EURUSD=0.0001,AUDUSD=0.0001,CHFJPY=0.01,EURGBP=0.0001,GBPUSD=0.0001,
              NZDUSD=0.0001,USDCAD=0.0001,USDCHF=0.0001,XAGUSD=0.01)
  return(as.numeric(symbols[symbol]))
}
#result = rangeAfterNbarforMbarFilter(pricedata,up=F,condition=6,m=3)

timeframe = c(5,15,30,60)
upordown = c(T,F)
conditiondays = 3:6
sepbars = 1:5
reports = list()
reports_index = 1
symbol = 'USDCAD' 
truePoint = getTheSmallestPoint(symbol)
if(is.na(truePoint)) truePoint = 1
for(i in 1:length(timeframe))
{
  pricedata = to.period(priceData,'minutes',timeframe[i])
  #映射的大周期
  longprice =getLongPeriodDataAndMap(timeframe[i],pricedata)
  longpricedata = longprice[[1]]
  shortmaptolong = longprice[[2]]
  for(i1 in 1 : length(upordown))
  {
    upflag = upordown[i1]
    for(i2 in 1 : length(conditiondays))
    {
      conditionday = conditiondays[i2]
      for(i3 in 1:length(sepbars))
      {
        sepbar = sepbars[i3]
        result = rangeAfterNbarforMbarFilter(pricedata,up=upflag,condition=conditionday,m=sepbar,filter=filterMaParentFrame(pricedata,longpricedata,shortmaptolong))
        point = result$point
        totalpoint = sum(point)
        pratio = length(point[point>0]) / length(point)
        nratio = length(point[point<0]) / length(point)
        totalcount = length(point)
        truetotalpoint = totalpoint/truePoint
        meanpoint = truetotalpoint /  totalcount
        if(pratio> 0.55 && totalpoint >0  &&  meanpoint > 10) # next m up
        {
          if(upflag == T) text = 'up' else text = 'down'
          r1 = paste("timeframe is : ", timeframe[i])
          r2 = paste("after ",conditionday," bars ", text)
          r3 = paste("we hold for :",sepbar," bars ")
          r4 = paste("up ratio is : " ,format(pratio,digits=3))
          r5 = paste("we get total nominal points : ",format(totalpoint))
          r6 = paste("we have  total  points : ",format( truetotalpoint))
          r7 = paste("we have total trade nums : ",totalcount)
          report =paste(r1,r2,r3,r4,r5,r6,r7)
          reports[[reports_index]] = report
          reports_index = reports_index + 1
          
        }
        if(nratio> 0.55 && totalpoint <0 && meanpoint < -10) # next m down
        {
          if(upflag == T) text = 'up' else text = 'down'
          r1 = paste("timeframe is : ", timeframe[i])
          r2 = paste("after ",conditionday," bars ", text)
          r3 = paste("we hold for :",sepbar," bars ")
          r4 = paste("down ratio is : " ,format(nratio,digits=3))
          r5 = paste("we get total nominal points : ",format(totalpoint))
          r6 = paste("we have total  points : ",format( truetotalpoint))
          r7 = paste("we have total trade nums : ",totalcount)
          report =paste(r1,r2,r3,r4,r5,r6,r7)
          reports[[reports_index]] = report
          reports_index = reports_index + 1
        }
        
      }
    }
  }
  
}

