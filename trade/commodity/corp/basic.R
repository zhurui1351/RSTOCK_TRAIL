rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R',encoding='utf8')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R',encoding='utf8')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/eventAnalysis',encoding='utf8')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/SNPACKAGE/R',encoding='utf8')

doubo_m = read_m_1m_taobao()
douyou_m = read_y_1m_taobao()
corp_m = read_c_1m_taobao()
dou1_m = read_s1_1m_taobao()

dou1_day = to_day(dou1_m)
doubo_day = to_day(doubo_m)
douyou_day = to_day(douyou_m)
corp_day = to_day(corp_m)

#sn 分析
data = corp_day
testMonthPeriod(data[,1:4],detail = T,from = '1990',to='2016',strict=F)

newyeardays =as.Date(c('2005-02-09','2006-01-29','2007-02-18','2008-02-07','2009-01-26','2010-02-14','2011-02-03',
                '2012-01-23','2013-02-10','2014-01-30','2015-02-19','2016-02-08'))

for(i in 1:length(newyeardays))
{
  d = newyeardays[i]
  start  = d - 30
  end = d + 30
  print(d)
  chartSeries(data[paste(start,end,sep = '/')])
  readline('next')
}