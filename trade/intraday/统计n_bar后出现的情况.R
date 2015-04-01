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