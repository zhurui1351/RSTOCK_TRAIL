rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/eventAnalysis')

dou1 = read_dou1_d_wind()
dou1$votile = dou1$Close - dou1$Open
dou1$maxvotile = dou1$High - dou1$Low

#A股冲击
shindex = read_sh000001()
shindex$votitleratio = Delt(shindex$Close)#(shindex$Close - shindex$Open) / shindex$Open

dates = index(shindex)[which(shindex$votitleratio  <  -0.05)]
basicEventStatic(dou1[,1:4],dates,30,1:30)

dates = index(shindex)[which(shindex$votitleratio  >  0.05)]
basicEventStatic(dou1[,1:4],dates,30,2)

#汇市冲击
usdcny_d = readusdcny_d()
usdcny_d_change = diff(usdcny_d) * 10000

#贬值
dates =index(usdcny_d_change[usdcny_d_change > 100]) 
basicEventStatic(dou1[,1:4],dates,30,1:10)
#升值
dates =index(usdcny_d_change[usdcny_d_change < -100]) 
basicEventStatic(dou1[,1:4],dates,30,2)

#价格异常波动
dou1_p = na.omit(dou1[,c('Open','High','Low','Close','votile')])
dou1_p$abssmav = lag(SMA(abs(dou1_p$votile),30,na.rm=T))
dou1_p$atr = ATR(dou1_p[,c('Open','High','Low','Close')],30)$atr
votile_change = dou1_p$votile/dou1_p$atr
dates = index(votile_change[votile_change < -2]) 
result = basicEventStatic(dou1[,1:4],dates,30,1)

dates = index(votile_change[votile_change > 2]) 
result = basicEventStatic(dou1[,1:4],dates,30,1)

#隔夜美豆异常波动
s1_d = read_s1_d_wind()
s1_d$votile =  s1_d$Close - s1_d$Open
s1_p = na.omit(s1_d[,c('Open','High','Low','Close','votile')])
s1_p$abssmav = lag(SMA(abs(s1_p$votile),30,na.rm=T))
votile_change = s1_p$votile/s1_p$abssmav
dates = index(votile_change[votile_change < -1.5])

result = basicEventStatic(dou1[,1:4],dates,30,1)

dates = index(votile_change[votile_change > 1.5])
basicEventStatic(dou1[,1:4],dates,30,2)


#经济事件 社会政治事件 自然事件 疫情 新奥尔良飓风灾难 hurricanes 季节性 气候
#日内特征
pricedata = collectdatafromtaobao()
#日内数据是2003年8月19号以后
records = data.frame()
eventdates = dates[dates>='2004-01-01']
mdates = unique(as.Date(index(pricedata)))



for(day in as.character(eventdates))
{
  if(is.element(as.Date(day),mdates))
  {
    i = which(mdates == as.Date(day))
    
    tmpperd = paste(paste(day,'21:00:00'),paste(day,'23:59:00'),sep='/')
    if(nrow(pricedata[tmpperd]) == 0)
    {
      i = i + 1
    }
    if(i > length(mdates)) next;
  }else
  {
     i =  findInterval(as.Date(day),mdates)
     if(i == 0) next;
  }
  testday = as.character(mdates[i])
  startdaytime = paste(testday,'09:00:00')
  enddaytime =  paste(testday,'15:00:00')
  startnighttime  = paste(testday,'21:00:00')
  endnighttime = paste(testday,'02:30:00')
  perdday = paste(startdaytime,endtime,sep='/')
  preday = as.character(mdates[i - 1])
  testmprice = pricedata[perdday]
  if(nrow(testmprice) == 0) next
  atr = as.numeric(dou1_p[preday]$atr)
  opentime = index(testmprice)[1]
  openprice =as.numeric(testmprice[1,]$Open) 
  initstop = openprice - atr
  tradeflag = F
  for(i in 2:nrow(testmprice))
  {
    if(i == nrow(testmprice))
    {
      closeprice = as.numeric(testmprice[i,]$Open)
      closetime = index(testmprice)[i]
      tradeflag = T
    }
    else
    {
      clprice = testmprice[i,]$Close
      if(clprice < initstop) 
      {
        closeprice = as.numeric(testmprice[i+1,]$Open)
        closetime = index(testmprice)[i+1]
        tradeflag = T
      }
    }
    if(tradeflag)
    {
      r = data.frame(opentime = opentime,closetime=closetime,openp = openprice,closep = closeprice,type='long')
      records = rbind(records,r)
      break
    }
  
  }
}
