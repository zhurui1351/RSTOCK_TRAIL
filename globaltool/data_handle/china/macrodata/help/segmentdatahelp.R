interestrate_1y_f_d_seg = function(perd,indicator)
{
  mydata = indicator[perd]
  if(nrow(mydata) == 0)
    return(NA)
  flag = as.numeric(mydata[nrow(mydata),]) - as.numeric(mydata[1,])
  if(flag > 0)
    return('上涨')
  else if(flag == 0)
    return('不变')
  else
    return('下降')
}


exchg_usd_rmb_m_seg = function(perd,indicator)
{
  mydata = indicator[perd]
  if(nrow(mydata) == 0)
    return(NA)
  flag = as.numeric(mydata[nrow(mydata),]) - as.numeric(mydata[1,])
  if(flag > 0.003)
    return('贬值')
  else if(flag < 0.003 && flag > -0.003)
    return('不变')
  else
    return('升值')
}

cpi_m2m_m_seg = function(perd,indicator)
{
  mydata = indicator[perd]
  if(nrow(mydata) == 0)
    return(NA)
  flag = as.numeric(mydata[nrow(mydata),]) - as.numeric(mydata[1,])
  if(flag > 0.2)
    return('上涨')
  else if(flag < 0.2 && flag > -0.2)
    return('不变')
  else
    return('下降')
}

cci_m_seg = function(perd,indicator)
{
  mydata = indicator[perd]
  if(nrow(mydata) == 0)
    return(NA)
  flag = as.numeric(mydata[nrow(mydata),]) - as.numeric(mydata[1,])
  if(flag > 0.5)
    return('上涨')
  else if(flag < 0.5 && flag > -0.5)
    return('不变')
  else
    return('下降')
}

emp_m2m_q_seg = function(perd,indicator)
{
  {
    mydata = indicator[perd]
    if(nrow(mydata) == 0)
      return(NA)
    flag = as.numeric(mydata[nrow(mydata),]) - as.numeric(mydata[1,])
    if(flag > 20)
      return('上涨')
    else if(flag < 20 && flag > -20)
      return('不变')
    else
      return('下降')
  }
}