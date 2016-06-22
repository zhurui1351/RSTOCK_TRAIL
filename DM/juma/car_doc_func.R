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
    r= data.frame(cusno = no,flag = flag)
    cusflag = rbind(cusflag,r)
  }
  return(cusflag)
}