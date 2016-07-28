cus_flag_func = function(cusdt_all,orderdt_all,enddate)
{
  cusdt = subset(cusdt_all,create_date <= enddate)
  orderdt = subset(orderdt_all,create_date <= enddate)
  cusflag = data.frame()
  cusnos = na.omit(cusdt$user_id)
  for(no in cusnos)
  {
    cusorders = subset(orderdt,orderdt$customer_id == no)
    if(nrow(cusorders) == 0) next
    
    dates = as.Date(cusorders$create_date[order(as.Date(cusorders$create_date),decreasing = F)]) 
    firstdate = dates[1]
    lastdate = dates[length(dates)]
    num = length(dates)
    
    #首次使用45天以内，只使用过一次
    if(num == 1 && firstdate >= (enddate - 45) )
    {
      flag = '体验用户'
    }
    #只使用过一次，首次使用在46-60天以内
    else if(num == 1 && (firstdate >= (enddate - 60)) && (firstdate < (enddate - 45)))
    {
      flag = '危险体验用户'
    }
    #使用两次以上,首次在60天以内最近一次在45天以内
    else if(num >= 2 && firstdate >= (enddate - 60) && lastdate >= (enddate - 45))
    {
      flag = '激活用户'
    }
    #使用两次以上，首次使用60天以内，最近一次在45天到60天
    else if(num >= 2 && (firstdate >= (enddate - 60)) && (lastdate < (enddate - 45)) && (lastdate >= (enddate - 60)) )
    {
      flag = '危险激活用户'
    }
    #只使用过一次，首次时间在60天以上
    else if(num == 1 &&  firstdate < (enddate - 60) )
    {
      flag = '流失体验用户'
    }
    #首次时间60天以上，两次以上，最近一次在45天以内
    else if(num >= 2 && firstdate < (enddate - 60)  && (lastdate >= (enddate - 45)) )
    {
      flag = '留存用户'
    }
    #首次使用60天以上，两次以上，最近一次在46 - 60天
    else if(num >= 2 && firstdate < (enddate - 60)  && (lastdate < (enddate - 45))  && (lastdate >= (enddate - 60)) )
    {
      flag = '危险用户'
    }
    #首次使用在60天以上，两次以上，最近一次60天以上
    else if(num >= 2 && firstdate < (enddate - 60)  && (lastdate < (enddate - 60)) )
    {
      flag = '流失用户'
    }
    else
    {
      flag = NA
    }
    
    r= data.frame(cusno = no,flag = flag,lastdate=lastdate)
    cusflag = rbind(cusflag,r)
  }
  return(cusflag)
}


month_live = function(cusdt,orderdt,start,end)
{
  month_date = as.Date(start)
  old_cus = subset(cusdt,create_date<month_date)$user_id
  old_cus_num = length(old_cus)
  
  month_date_end = as.Date(end)
  n_order = subset(orderdt,create_date>=month_date & create_date<=month_date_end)
  n_cus = n_order$customer_id
  reaccess_cus = na.omit(old_cus[old_cus %in% n_cus])
  reaccess_cus_num = length(reaccess_cus)
  rate = ifelse(old_cus_num == 0,0,reaccess_cus_num / old_cus_num)
  return(rate)
}

month_live_surv = function(cusdt,orderdt,start,end)
{
  month_date = as.Date(start)
  cusflag = cus_flag_func(cusdt,orderdt,start)
  if(nrow(cusflag) == 0) return(0)
  old_cus = subset(cusflag,flag == '留存用户')$cusno
  old_cus_num = length(old_cus)
  
  month_date_end = as.Date(end)
  n_order = subset(orderdt,create_date>=month_date & create_date<=month_date_end)
  n_cus = n_order$customer_id
  reaccess_cus = na.omit(old_cus[old_cus %in% n_cus])
  reaccess_cus_num = length(reaccess_cus)
  rate = ifelse(old_cus_num == 0,0,reaccess_cus_num / old_cus_num)
  return(rate)
}

survival_rate = function(cusflag)
{
  dt_cus_flag = aggregate(cusflag$flag,by = list(cusflag$flag),length)
  colnames(dt_cus_flag) = c('客户类别','客户量')
  dt_cus_flag$比例 = dt_cus_flag$客户量/nrow(cusflag)
  
  survrate = subset(dt_cus_flag,客户类别=='留存用户')[,'客户量'] / sum(subset(dt_cus_flag,客户类别 %in% c('留存用户','危险用户','流失体验用户','流失用户'))[,'客户量']) 
  return(survrate)
  
}

