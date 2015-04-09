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
 # for(i in 1 : length(s))
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
        return(list(ratio=ratio_list,point=point_list,count=count_condition_happen))
      }
      if(count==condition && cumrange>= range)
      {
        count_condition_happen = count_condition_happen + 1
        nextrange = 0
        point = 0
        for( x in 1:m)
        {
          i = i + 1
          nextrange = nextrange + ratio[i]
          point = point + s[i]
        }
        ratio_list[count_condition_happen] = nextrange
        point_list[count_condition_happen] = point
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
        return(list(ratio=ratio_list,point=point_list,count=count_condition_happen))
      }
      if(count==condition  && cumrange <= range)
      {
        count_condition_happen = count_condition_happen + 1
        nextrange = 0
        point = 0
        for( x in 1:m)
        {
          i = i + 1
          nextrange = nextrange + ratio[i]
          point = point + s[i]
        }
        ratio_list[count_condition_happen] = nextrange
        point_list[count_condition_happen] = point
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
  return(list(ratio=ratio_list,point=point_list,count=count_condition_happen))
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

result = rangeAfterNbarforMbar(pricedata,up=F,condition=5,m=1)
point = result$point
sum(point)
length(point[point>0]) / length(point)
length(point)
