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
  logger <<- new.env()
  logger$record <- data.frame()
  
  date = index(pricedata)
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
      #符合条件
      if(count==condition && cumrange>= range)
      {
        count_condition_happen = count_condition_happen + 1
        nextrange = 0
        point = 0
        point_list[count_condition_happen] = cl[i+m] - op[i]
        
        logger$record<-rbind(logger$record,data.frame(date=date[i],price=op[i],type='first'))          
        logger$record<-rbind(logger$record,data.frame(date=date[i+m],price=cl[i+m] ,type='second'))   
        
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
        point_list[count_condition_happen] = cl[i+m] - op[i]
        
        logger$record<-rbind(logger$record,data.frame(date=date[i],price=op[i],type='first'))          
        logger$record<-rbind(logger$record,data.frame(date=date[i+m],price=cl[i+m] ,type='second'))   
        
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


