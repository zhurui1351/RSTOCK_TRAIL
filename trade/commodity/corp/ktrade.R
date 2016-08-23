rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R',encoding='utf8')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R',encoding='utf8')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/eventAnalysis',encoding='utf8')

doubo_m = read_m_1m_taobao()
douyou_m = read_y_1m_taobao()
corp_m = read_c_1m_taobao()
dou1_m = read_s1_1m_taobao()

dou1_day = to_day(dou1_m)
doubo_day = to_day(doubo_m)
douyou_day = to_day(douyou_m)
corp_day = to_day(corp_m)



#对齐开盘时间
index(doubo_m) = index(doubo_m) - 60
index(douyou_m) = index(douyou_m) - 60
index(corp_m) = index(corp_m) - 60
index(dou1_m) = index(dou1_m) - 60

doubo_15m = to_minutes(doubo_m,k=15)
douyou_15m = to_minutes(douyou_m,k=15)
corp_15m = to_minutes(corp_m,k=15)
dou1_15m = to_minutes(dou1_m,k=15)


#开盘n分钟跳空,大阴大阳
#考虑每个开盘瞬间的跳空情况，相关市场跳空情况，跳空后第一根k线涨跌情况，不同时间框架下的
#跳空情况，判定条件是到一定时期收盘是否会有关闭缺口的迹象
pricedata_m = corp_15m
pricedata_m$smashort = lag(SMA(Cl(pricedata_m),3),1)
pricedata_m$smalong= lag(SMA(Cl(pricedata_m),10),1)

pricedata = corp_day
days = as.character(unique(as.Date(index(pricedata))))
alltime = index(pricedata_m)
time = '09:00:00'
time1 = '15:00:00'
stop = 10
trail_profit = 5
result = data.frame()
for(day in days[2:length(days)])
{
  iday = which(days == day)
  preday = days[iday - 1]
  opentime = paste(day,time)
  endtime = paste(day,time1)
  
  tradetime = paste(opentime,endtime,sep='/')
  daydata = pricedata_m[tradetime]
  
  idx = which(alltime == opentime)
  endidx = which(alltime == endtime)
  
  if(length(idx) == 0) next
  precloseidx = idx - 1
  
  preday_votile = as.numeric(Cl(pricedata[preday]) - Op(pricedata[preday]))
  
  open = as.numeric(Op(pricedata_m[idx,]))
  enter = as.numeric(Op(daydata[2,]))
  close = as.numeric(Cl(pricedata_m[idx,]))
  starthigh =  as.numeric(Hi(pricedata_m[idx,]))
  startlow =  as.numeric(Lo(pricedata_m[idx,]))
  
  high = as.numeric(Hi(pricedata[day]))
  low = as.numeric(Lo(pricedata[day]))
  
  prehigh = as.numeric(Hi(pricedata_m[precloseidx,]))
  prelow = as.numeric(Lo(pricedata_m[precloseidx,]))
  
  smashort = as.numeric(pricedata_m[idx,]$smashort)
  smalong = as.numeric(pricedata_m[idx,]$smalong)
  
  upgap = open - prehigh
  downgap = prelow - open
  
  flag = as.numeric(Cl(daydata[1,])) - as.numeric(Op(daydata[1,]))

  if(downgap > 5)
  {
    type = 'up'
    for(i in 2 : nrow(daydata))
    {
      if(i == nrow(daydata))
      {
        out = as.numeric(Cl(daydata[i,]))
        outtime = index(daydata[i,])
        break
      }
      s_low = as.numeric(Lo(daydata[i,]))
      s_high = as.numeric(Hi(daydata[i,]))
      if((enter-s_low) > stop)
      {
        out = enter - stop
        outtime = index(daydata[i,])
        
        break
      }
      if((s_high - enter) > trail_profit)
      {
        out = enter + trail_profit
        outtime = index(daydata[i,])
        
        break
      }
    }
  }
  else if(upgap > 5)
  {
    type = 'down'
    for(i in 2 : nrow(daydata))
    {
      if(i == nrow(daydata))
      {
        out = as.numeric(Cl(daydata[i,]))
        outtime = index(daydata[i,])
        
        break
      }
      s_low = as.numeric(Lo(daydata[i,]))
      s_high = as.numeric(Hi(daydata[i,]))
      if((s_high - enter) > stop)
      {
        out = enter + stop
        outtime = index(daydata[i,])
        
        break
      }
      if((enter-s_low) > trail_profit)
      {
        out = enter - trail_profit
        outtime = index(daydata[i,])
        
        break
      }
    }
  }
  else
  {
    type = ''
    next
  }
  
  r = data.frame(day,type =type,flag=flag,open=enter,out=out,outtime = outtime,high=high-open,low=low-open)
  result = rbind(result,r)
 
}

result$profit = ifelse(result$type == 'up',result$out-result$open,result$open-result$out)
profit = result$profit
length(profit[profit>0]) / length(profit)

result$flag = ifelse(result$flag >0,'1',ifelse(result$flag<0,'-1','0'))
result$profit = ifelse(result$profit >0,'1','-1')

i = which(result$flag == 0)
result = result[-i,]

result$profit = as.factor(result$profit)
result$type = as.factor(result$type)
ct <- rpart.control(xval=10, minsplit=20, cp=0.001)  
fit = randomForest(profit ~ type + flag,data = result)
rpart.plot(fit)
pr = predict(fit,type='class')
pt = table(result$profit,pr)
precison = (pt[1,1] + pt[2,2]) / sum(pt)
