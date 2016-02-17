source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R',encoding = 'utf8')

readmacrodata_jp = function(path = 'D:/data/各国数据/japan/macro/')
{
  e = parent.env(environment())
  tableforread = data.frame()
  r = data.frame(filename = 'CPI.csv',var = 'cpi_m2m_m,cpi_mRm_m,cpi_y2y_y',fun='cpi_fun')
  tableforread = rbind(tableforread,r )
  r = data.frame(filename = 'CPI当月同比.csv',var = 'cpi_m2m_m',fun='cpi_m2m_m_fun' )
  tableforread = rbind(tableforread,r )
  for(i in 1:nrow(tableforread))
  {
    r = tableforread[i,]
    f = paste(path,r[,'filename'],sep='')
    tb = read.csv(f)
    fun = as.character(r[,'fun'])
    var = as.character(r[,'var'])
    var = unlist(strsplit(var,','))
    if(fun =='')
    {
      assign(var,tb,envir=e)
    }
    else
    {
      tmpcall = call(fun,tb)
      tmp = eval(tmpcall)
      #assign(var,tmp)
      for(j in 1 : length(var))
      {
        assign(var[j],tmp[[j]],envir=e)
      }
    }
  }
  return(tableforread)
}
