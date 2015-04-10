# n个bar连涨连跌后的涨跌情况
performanceAfterNbar <- function(s,up=T,condition = 3)
{
  pre = F
  count = 0
  
  result = c(0)
  count_condition_happen = 0
  for(i in 1 : length(s))
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
    }
    else if(up==F && pre ==T && s[i] < 0)
    {
      count =  count + 1
    }
    else
    {
      if(count>=condition)
      {
        count_condition_happen = count_condition_happen + 1
      }
      
      if(count > condition)
      {
        if(is.na(result[count-condition]))
        {
          result[count-condition] = 1
        }
        else
        {
          result[count-condition] = result[count-condition] + 1
        }
        
      }
      pre = F
      count = 0
    }
  }
  #处理末尾的情况
  if(up==T && pre ==T && s[i] >0)
  {
    
    if(count>=condition)
    {
      count_condition_happen = count_condition_happen + 1
    }
    
    if(count > condition)
    {
      if(is.na(result[count-condition]))
      {
        result[count-condition] = 1
      }
      else
      {
        result[count-condition] = result[count-condition] + 1
      }
      
    }
  }
  if(up==F && pre ==T && s[i] < 0)
  {
    if(count>=condition)
    {
      count_condition_happen = count_condition_happen + 1
    }
    
    if(count > condition)
    {
      if(is.na(result[count-condition]))
      {
        result[count-condition] = 1
      }
      else
      {
        result[count-condition] = result[count-condition] + 1
      }
      
    }
  }
  return(list(result=result,count=count_condition_happen))
}


performanceAfterNbarThenReverse <- function(s,up=T,condition = 3)
{
  pre = F
  count = 0
  
  result = 0
  count_condition_happen = 0
  for(i in 1 : length(s))
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
    }
    else if(up==F && pre ==T && s[i] < 0)
    {
      count =  count + 1
    }
    else
    {
      if(count>=condition)
      {
        count_condition_happen = count_condition_happen + 1
      }  
      if(count == condition )
      {
        result = result + 1
      }
      count = 0
      pre = F
    }
  }
  return(list(result=result,count=count_condition_happen))
}
#连续的
rangeAfterNbarforMbar <- function(pricedata,up=T,condition = 3,range=0,m=1)
{
  s = as.numeric(Cl(pricedata) - Op(pricedata)) 
  op = as.numeric(Op(pricedata)) 
  cl = as.numeric(Cl(pricedata)) 
  
  ratio = as.numeric(Cl(pricedata) - Op(pricedata)) / as.numeric(Op(pricedata))
  ratio[is.infinite(ratio)==T] = 0 
  ratio[is.na(ratio)==T] = 0
  pre = F
  count = 0
  
  cumrange = 0
  ratio_list = c(0)
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
      cumrange = cumrange + s[i]
      #return if do not have enough data
      if(i + m > length(s))
      {
        return(list(point=point_list,count=count_condition_happen))
      }
      if(count==condition && cumrange>= range)
      {
        count_condition_happen = count_condition_happen + 1
        nextrange = 0
        point = 0
      #  for( x in 1:m)
      #  {
       #   i = i + 1
        #  nextrange = nextrange + ratio[i]
         # point = point + s[i]
      #  }
      #  ratio_list[count_condition_happen] = nextrange
        point_list[count_condition_happen] = cl[i+m] - op[i]
        i = i + m + 1
        cumrange = 0
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
      cumrange = cumrange + s[i] 
      #return if do not have enough data
      if(i + m > length(s))
      {
        return(list(point=point_list,count=count_condition_happen))
      }
      if(count==condition  && cumrange <= -range)
      {
        count_condition_happen = count_condition_happen + 1
        nextrange = 0
        point = 0
    #    for( x in 1:m)
     #   {
      #    i = i + 1
       #   nextrange = nextrange + ratio[i]
        #  point = point + s[i]
     #   }
     #   ratio_list[count_condition_happen] = nextrange
        point_list[count_condition_happen] = cl[i+m] - op[i]
        i = i + m + 1
        cumrange = 0
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
      cumrange = 0
      pre = F
      count = 0
      i = i +1
    }
  }
  return(list(point=point_list,count=count_condition_happen))
}

require(quantmod)
path="D:/data/dest"
files<-dir(path)
#files=c('SZ000630.txt')
#f = files[1]
i = 1
ratio_list = c(0)
profit_list =c(0)
mean_profit_list = c(0)
for(f in files)
{
  fname<-file.path(path,f)
  pricedata<-read.zoo(fname,header=FALSE, format ="%m/%d/%Y",sep="\t",index.column=1)
  colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
  
  #bar = as.numeric(Cl(pricedata)-Op(pricedata))
  #result = performanceAfterNbarThenReverse(bar,up=F,condition=3)
  result = rangeAfterNbarforMbar(pricedata,up=F)
  #ratio = result$result / result$count
  profit = sum(result$result)
  profit_list[i] = profit
  
  x = result$result
  ratio = length(x[x>0]) / length(x)
  ratio_list[i] = ratio
  
  mean_profit = mean(result$result)
  mean_profit_list[i] = mean_profit
  i = i +1  
}

result = rangeAfterNbarforMbar(pricedata,up=F,condition=6,m=3)

getTheSmallestPoint <- function(symbol)
{
   symbols = c(USDJPY=0.01,AUDJPY=0.01,EURUSD=0.0001,AUDUSD=0.0001,CHFJPY=0.01)
   return(as.numeric(symbols[symbol]))
}

timeframe = c(5,15,30,60)
upordown = c(T,F)
conditiondays = 3:6
sepbars = 1:5
reports = list()
reports_index = 1
symbol = 'EURGBP' 
truePoint = getTheSmallestPoint(symbol)
if(is.na(truePoint)) truePoint = 1
for(i in 1:length(timeframe))
{
  pricedata = to.period(priceData,'minutes',timeframe[i])
  for(i1 in 1 : length(upordown))
  {
    upflag = upordown[i1]
    for(i2 in 1 : length(conditiondays))
    {
      conditionday = conditiondays[i2]
      for(i3 in 1:length(sepbars))
      {
        sepbar = sepbars[i3]
        result = rangeAfterNbarforMbar(pricedata,up=upflag,condition=conditionday,m=sepbar)
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

