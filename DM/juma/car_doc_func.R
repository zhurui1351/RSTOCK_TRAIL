#客户打上活跃度标签
cus_flag_func = function(cusdt_all,orderdt_all,enddate)
{
  cusdt = subset(cusdt_all,首次服务日期 <= enddate)
  orderdt = subset(orderdt_all,服务日期 <= enddate)
  cusflag = data.frame()
  cusnos = na.omit(cusdt$序号)
  for(no in cusnos)
  {
    cusorders = subset(orderdt,orderdt$cusno == no)
    if(nrow(cusorders) == 0) next
    
    dates = as.Date(cusorders$服务日期[order(as.Date(cusorders$服务日期),decreasing = F)]) 
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

survival_rate = function(cusflag)
{
  dt_cus_flag = aggregate(cusflag$flag,by = list(cusflag$flag),length)
  colnames(dt_cus_flag) = c('客户类别','客户量')
  dt_cus_flag$比例 = dt_cus_flag$客户量/nrow(cusflag)
  
  survrate = subset(dt_cus_flag,客户类别=='留存用户')[,'客户量'] / sum(subset(dt_cus_flag,客户类别 %in% c('留存用户','危险用户','流失体验用户','流失用户'))[,'客户量']) 
  return(survrate)
  
}

active_rate = function(cusdt_all,orderdt_all,days=30,enddate)
{
  cusdt = subset(cusdt_all,首次服务日期 <= enddate)
  orderdt = subset(orderdt_all,服务日期 <= enddate)
  cus_flag = data.frame()
  cusnos = na.omit(cusdt$序号)
  for(no in cusnos)
  {
    cusorders = subset(orderdt,orderdt$cusno == no)
    if(nrow(cusorders) == 0) next
    else if(nrow(cusorders) == 1) {flag = 'no'}
    else
    {
      dates = as.Date(cusorders$服务日期[order(as.Date(cusorders$服务日期),decreasing = F)]) 
      gap = as.numeric(dates[2] - dates[1])
      if(gap <= days) 
      {
        flag = 'yes'
      }
      else
      {
        flag = 'no'
      }
    }
    
    r = data.frame(cusno=no,flag=flag)
    cus_flag = rbind(cus_flag,r)
  }
  
  rate = subset(cus_flag,cus_flag[,2] == 'yes')
  rate = nrow(rate) / nrow(cus_flag)
  return(rate)
}

new_num_period = function(cusdt_all,start,end)
{
  n_cus = subset(cusdt_all,首次服务日期 >= start & 首次服务日期<=end)
  num = length(unique(n_cus$序号))
  return(num)
}

relive_num_period = function(cusdt_all,orderdt_all,start,end)
{
  n_order = subset(orderdt_all,服务日期 >= start & 服务日期<=end)
  
  actcus = unique(na.omit(n_order$cusno))
  
  oldcus = subset(cusdt_all,as.Date(cusdt$首次服务日期) < start )$序号
  reaccesscus = actcus[actcus %in% oldcus]
  
  cus_flag_preact = cus_flag_func(cusdt_all,orderdt_all,start)
  death_cus = subset(cus_flag_preact,flag %in% c('流失用户','流失体验用户'))
  
  death_to_live = reaccesscus[which(reaccesscus %in% death_cus$cusno)]
  death_to_live_num = length(death_to_live)
  return(death_to_live_num)
}