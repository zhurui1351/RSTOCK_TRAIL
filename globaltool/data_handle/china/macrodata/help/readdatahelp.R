interestrate_1y_f_d_fun = function(tb)
{
  #做成连续利率
  tmp = data.frame()
  predate = tb[1,1]
  preval = tb[1,2]
  for(i in 2:nrow(tb))
  {
    r = tb[i,]
    curdate = r[,1]
    curval = r[,2]
    dates = seq(as.Date(predate),as.Date(curdate),by='day')
    dates = dates[1:(length(dates)-1)]
    vals = rep(preval,length(dates))
    
    rtmp = data.frame(dates,vals)
    tmp = rbind(tmp,rtmp)
    
    predate = curdate
    preval = curval
  }
  
  curdate = strftime(now(),'%Y-%m-%d')
  
  dates = seq(as.Date(predate),as.Date(curdate),by='day')
  dates = dates[1:(length(dates)-1)]
  vals = rep(preval,length(dates))
  
  rtmp = data.frame(dates,vals)
  tmp = rbind(tmp,rtmp)
  
  tmp = xts(tmp[,2],order.by=tmp[,1])
  
  return(tmp)
}

fixdates_and_transfer = function(tb)
{
  dates = as.Date(sapply(tb[,1], getMonthEnd))
  tmp = as.xts(tb[,2],order.by = dates)
  return(tmp)
}

cpi_m2m_m_fun = function(tb)
{
  tmp = fixdates_and_transfer(tb)
  return(tmp)
}

cpi_mRm_m_fun = function(tb)
{
  tmp = fixdates_and_transfer(tb)
  return(tmp)
}

gdp_q2q_q_fun = function(tb)
{
  tmp = fixdates_and_transfer(tb)
  return(tmp)
}

gdp_sum_q2q_q_fun = function(tb)
{
  tmp = fixdates_and_transfer(tb)
  return(tmp)
}

m2_m_fun = function(tb)
{
  tb2 = as.numeric(gsub(',','',tb[,2]))
  tmp = fixdates_and_transfer(data.frame(tb[,1],tb2))
  return(tmp)
}

ppi_m2m_m_fun = function(tb)
{
  tmp = fixdates_and_transfer(tb)
  return(tmp)
}

m2_m2m_m_fun = function(tb)
{
  tmp = fixdates_and_transfer(tb)
  return(tmp)
}

emp_m2m_q_fun = function(tb)
{
  tb2 = as.numeric(gsub(',','',tb[,2]))
  tmp = fixdates_and_transfer(data.frame(tb[,1],tb2))
  return(tmp)
}

exchg_usd_rmb_m_fun = function(tb)
{
  tmp = fixdates_and_transfer(tb)
  return(tmp)
}

cci_m_fun = function(tb)
{
  tmp = fixdates_and_transfer(tb)
  return(tmp)
}
