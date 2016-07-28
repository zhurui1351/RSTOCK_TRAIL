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

getmaxcusno = function(cusdt)
{
  if(nrow(cusdt) == 0) return(0)
  cusnos = cusdt$序号
  return(max(cusnos))
}

getCustomerfromOrder_increase = function(cusdt,orderdt,day)
{
  new_cusdt = cusdt
  order_day = subset(orderdt,服务日期 == day)
  for(i in 1:nrow(order_day))
  {
    r = order_day[i,]
    if(!is.null(new_cusdt$车牌号码) && !is.na(r[1,'车牌号']) && !is.null(r[1,'车牌号']) && nchar(r[1,'车牌号']) > 1)
    {
      subdt = subset(new_cusdt,车牌号码== r[1,'车牌号'] )[1,]
      if(!is.null(subdt) && nrow(subdt) > 0 && !is.na(subdt[,'序号']) && !is.null(subdt[,'序号']) )
      {
        next
      }
    }
    #按车牌无法索引到客户
    if(!is.null(new_cusdt$电话) && !is.na(r[1,'联系方式']) && !is.null(r[1,'联系方式']) && nchar(r[1,'联系方式']) > 1)
    {
      subdt = subset(new_cusdt,电话== r[1,'联系方式'] )[1,]
      if( nrow(subdt) > 0 && !is.na(subdt[,'序号']) && !is.null(subdt[,'序号']) )
      {
        next
      }
    }
    #按客户姓名
    if(!is.null(new_cusdt$客户姓名) && !is.na(r[1,'客户姓名']) && !is.null(r[1,'客户姓名']) && nchar(r[1,'客户姓名']) > 1)
    {
      subdt = subset(new_cusdt,客户姓名== r[1,'客户姓名'] )[1,]
      if( nrow(subdt) > 0 && !is.na(subdt[,'序号']) && !is.null(subdt[,'序号']) )
      {
        next
      }
    }
    
    #判定为新用户
    cusno = getmaxcusno(new_cusdt) + 1
    r_c = data.frame(序号=cusno,首次服务日期=day,客户姓名=r[1,'客户姓名'],电话=r[1,'联系方式'],车牌号码=r[1,'车牌号'],
                       VIN码='',车辆初次登记日期='', 车辆类型=r[1,'车辆类型'],核载人数='',发动机型号=r[1,'发动机型号'],
                       行驶里程.KM=r[1,'行驶里程.KM'],品牌=r[1,"车辆品牌"],车型=r[1,'车辆类型'],车辆长度=r[1,'车辆长度.米'],
                       入城证=r[1,'是否有入城证'],车牌颜色=r[1,'车牌照颜色'],保养地点=r[1,'服务地址'],保险公司=r[1,'保险公司'],保险到期日期=r[1,'保险到期日期'],
                       车辆类型.1='',保险险种=r$保险险种,保险情况='',是否挂靠='',挂靠.搬家公司='',是否考虑换车='',X='')
    
    new_cusdt = rbind(new_cusdt,r_c)
  }
  return(new_cusdt)
}

getCustomerfromOrder_all = function(cus_all,orderdt)
{
  days = unique(orderdt$服务日期)
  for(i in 1:length(days))
  {
    day = days[i]
    day = as.Date(day)
    cus = getCustomerfromOrder_increase(cus_all,orderdt,day)
    cus_all = cus
  }
  return(cus_all)
}


getCusinfobytype = function(cusnos,orderdt)
{
  actcus_info = data.frame()
  for(cus in cusnos)
  {
    cus_info = subset(cusdt,序号==cus)
    order_info = subset(orderdt,cusno == cus)
    dates = as.Date(order_info$服务日期[order(as.Date(order_info$服务日期),decreasing = F)]) 
    firstdate = dates[1]
    lastdate = dates[length(dates)]
    order_num = nrow(order_info)
    fee = sum(order_info$收费合计,na.rm = T)
    fee_mean = mean(order_info$收费合计,na.rm = T)
    max_fee = max(order_info$收费合计,na.rm = T)
    time_gap = diff(as.Date(order_info$服务日期))
    time_gap_mean = mean(time_gap)
    max_time_gap = max(time_gap)
    service = unique(order_info$服务项目)
    service = na.omit(gsub(',','、',service))
    service = strsplit(service,'、')
    service = unique(unlist(service))
    service_num = length(service)
    
    r = data.frame(客户号=cus,订单量=order_num,总花费=fee,客单价=fee_mean,最大消费=max_fee,平均服务间隔=time_gap_mean,
                      最大间隔=max_time_gap,服务范围=service_num,注册时间=firstdate,最近服务时间=lastdate)
    actcus_info = rbind(actcus_info,r)
  }
  return(actcus_info)
}

basic_user_stat = function(cusdt,orderdt)
{
  result = data.frame()
  days = unique(as.Date(orderdt$服务日期))
  for(i in 1:length(days))
  {
    day = days[i]
    cusflag = cus_flag_func(cusdt,orderdt,day)
    death_cus = subset(cusflag,flag %in% c('流失用户','流失体验用户'))
    death_cus_num = nrow(death_cus)
    
    cus = subset(cusdt,首次服务日期==day)
    orders = subset(orderdt,服务日期==day)
    cusnum = length(na.omit(cus$序号))
    oldcus = setdiff(orders$cusno,cus$序号)
    oldcusnum = length(oldcus)
    ordernum = nrow(orders)
    r = data.frame(日期=day,新增客户=cusnum,老客户=oldcusnum,订单量=ordernum,死亡用户数=death_cus_num)
    result = rbind(result,r)
  }
  return(result)
}
