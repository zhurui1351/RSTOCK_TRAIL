source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R',encoding = 'utf8')

readmacrodata_jp = function(path = 'D:/data/各国数据/japan/macro/')
{
  e = parent.env(environment())
  tableforread = data.frame()
  r = data.frame(filename = '1年整存整取利率.csv',var = 'interestrate_1y_f_d',fun='interestrate_1y_f_d_fun')
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = 'CPI当月同比.csv',var = 'cpi_m2m_m',fun='cpi_m2m_m_fun' )
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = 'CPI环比.csv',var = 'cpi_mRm_m',fun='cpi_mRm_m_fun' )
  tableforread = rbind(tableforread,r)
  r = data.frame(filename = 'GDP不变价当季同比.csv',var = 'gdp_q2q_q',fun='gdp_q2q_q_fun' )
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = 'GDP不变价累计同比.csv',var = 'gdp_sum_q2q_q',fun='gdp_sum_q2q_q_fun' )
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = 'M2.csv',var = 'm2_m',fun='m2_m_fun' )
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = 'M2同比.csv',var = 'm2_m2m_m',fun='m2_m2m_m_fun' )
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = 'PPI工业品当月同比.csv',var = 'ppi_m2m_m',fun='ppi_m2m_m_fun' )
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = '就业人数合计同比增减.csv',var = 'emp_m2m_q',fun='emp_m2m_q_fun' )
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = '平均汇率美元兑人民币.csv',var = 'exchg_usd_rmb_m',fun='exchg_usd_rmb_m_fun' )
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = '消费者信心指数.csv',var = 'cci_m',fun='cci_m_fun' )
  tableforread = rbind(tableforread,r )
  
  for(i in 1:nrow(tableforread))
  {
    r = tableforread[i,]
    f = paste(path,r[,'filename'],sep='')
    tb = read.csv(f)
    fun = as.character(r[,'fun'])
    var = as.character(r[,'var'])
    if(fun =='')
    {
      #assign(var,tb)
      assign(var,tb,envir=e)
    }
    else
    {
      tmpcall = call(fun,tb)
      tmp = eval(tmpcall)
      #assign(var,tmp)
      assign(var,tmp,envir=e)
    }
  }
  return(tableforread)
}
