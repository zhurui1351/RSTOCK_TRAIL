require('quantmod')
path = "D:/stock/FOREX"
reportpath = "D:/myreport_m"
f = 'SH000001_5ms.TXT'
fname = file.path(path,f)

priceData <- read.table(fname,sep='\t',header=T)
priceData=xts(priceData[2:7],order.by=as.POSIXct(priceData[,1]))
colnames(priceData) <- c("Open","High","Low","Close","Volume","Amount")

daydata = to.daily(priceData)
colnames(daydata) <- c("Open","High","Low","Close","Volume")

#取日数据
all_days = unique(strftime(index(priceData),'%Y-%m-%d'))
h_count = 0
l_count = 0
h_l_count = 0
leveltolow = c()
leveltohigh = c()
leveltoclose = c()
upfallthesame = 0
volatilerange =c()
for(day in all_days)
{
  high_in_day =as.numeric(Hi(daydata[day]))
  low_in_day = as.numeric(Lo(daydata[day]))
  open_in_day =as.numeric(Op(daydata[day]))
  close_in_day = as.numeric(Cl(daydata[day]))
  m_data = to.period(priceData[day],'minutes',30)
  high_in_m =as.numeric(Hi(m_data[1]))
  low_in_m = as.numeric(Lo(m_data[1]))
  open_in_m =as.numeric(Op(m_data[1]))
  close_in_m = as.numeric(Cl(m_data[1]))
  if(high_in_m == high_in_day)
  {
    h_count = h_count + 1
  }
  if(low_in_day == low_in_m)
  {
    l_count = l_count + 1
  }
  
  if(low_in_day == low_in_m && high_in_m == high_in_day )
  {
    h_l_count = h_l_count + 1
  }
  if(sign(close_in_m-open_in_m)==sign(close_in_day-open_in_day))
  {
    upfallthesame = upfallthesame + 1
  }
  
  leveltohigh = c(high_in_day - high_in_m,leveltohigh)
  leveltolow = c(low_in_day - low_in_m,leveltolow )
  leveltoclose = c(as.numeric(Cl(daydata[day])) - high_in_m , leveltoclose)
  volatilerange =c( (high_in_m - low_in_m)/(high_in_day - low_in_day),volatilerange)
}


