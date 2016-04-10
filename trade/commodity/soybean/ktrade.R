rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/include.R')
source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/readdata.R')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/eventAnalysis')
pricedata = collectdatafromtaobao()

days = as.character(unique(as.Date(index(pricedata))))

records = data.frame()
for(day in days)
{
  p = pricedata[day]
  if(nrow(p) ==0) next
  perdstart = paste(day,'09:00:00')
  perdend = paste(day,'15:00:00')
  perd = paste(perdstart,perdend,sep='/')
  nperd = nrow(p[perd])
  if(nperd == 0) next
  
  time1 = paste(day,'09:01:00')
  time2 = paste(day,'09:10:00')
  time3 = paste(day,'15:00:00')
  
  open = as.numeric(p[time1]$Open)
  end1 = as.numeric(p[time2]$Close)
  end2 = as.numeric(p[time3]$Close)
  r = data.frame(date=day,open = open,end1 = end1,end2=end2,sign1 = sign(end1 - open),sign2 = sign(end2 - open))
  records = rbind(records,r)
}

years = unique(substring(days,1,7))
anlysisframe = data.frame()
for(y in years)
{
  sub = subset(records,substring(date,1,7) == y)
  sub1 = subset(sub,sign1 == sign2)
  ratio = nrow(sub1) / nrow(sub)
  a = data.frame(year = y , ratio = ratio)
  anlysisframe = rbind(anlysisframe,a)
}

#最高最低点 时间分布统计 暂时只考虑9-15点
maxtime  = c()
mintime = c()
for(day in days)
{
  p = pricedata[day]
  if(nrow(p) ==0) next
  perdstart = paste(day,'09:00:00')
  perdend = paste(day,'15:00:00')
  perd = paste(perdstart,perdend,sep='/')
  nperd = nrow(p[perd])
  if(nperd == 0) next
  p = p[perd]
  maxp = max(p$High)
  maxi = which(p$High == maxp)
  maxi = unique(substring(as.character(index(p[maxi,])),12,13))
  maxtime = c(maxtime,maxi)
  
  minp = min(p$Low)
  mini = which(p$Low == minp)
  mini = unique(substring(as.character(index(p[mini,])),12,13))
  mintime = c(mintime,mini)
}
aggregate(maxtime,by = list(maxtime),length)
aggregate(mintime,by = list(mintime),length)

#回测
records = data.frame()
for(day in days)
{
  p = pricedata[day]
  if(nrow(p) ==0) next
  perdstart = paste(day,'09:00:00')
  perdend = paste(day,'15:00:00')
  perd = paste(perdstart,perdend,sep='/')
  nperd = nrow(p[perd])
  if(nperd == 0) next
  
  time1 = paste(day,'09:01:00')
  time2 = paste(day,'09:10:00')
  time3 = paste(day,'15:00:00')
  
  open = as.numeric(p[time1]$Open)
  end1 = as.numeric(p[time2]$Close)
  end2 = as.numeric(p[time3]$Close)
  type = ''
  if((end1 - open) < 0 )
  {
    type = 'short'
  }
  else if((end1 - open) == 0)
  {
    next
  }
  else
  {
    type = 'long'
  }
  
  i = which(as.character(index(p)) == time2) + 1
  enter = as.numeric(p[i,]$Open)
  if(type =='short')
  {
    stopprice = open + 20
    stopwin = enter - 10
  }
  else
  {
    stopprice = open - 20
    stopwin = enter + 10
    
  }
  isstop = F
  for(idx in i : nrow(p))
  {
    idxcl = as.numeric(p[idx,]$Close)
    if(type == 'short')
    {
      if(idxcl > stopprice)
      {
        isstop = T
        r = data.frame(time = index(p[idx,]),enter=enter,out =idxcl,isstop = isstop,type = type , profit = enter - idxcl )
        records = rbind(records,r)
        break
      }
      
      if(idxcl < stopwin)
      {
        isstop = T
        r = data.frame(time = index(p[idx,]),enter=enter,out =idxcl,isstop = isstop,type = type , profit = enter - idxcl )
        records = rbind(records,r)
        break
      }
    }
    else
    {
      if(idxcl < stopprice)
      {
        isstop = T
        r = data.frame(time = index(p[idx,]),enter=enter,out =idxcl,isstop = isstop,type = type , profit = idxcl - enter )
        records = rbind(records,r)
        break
      }
      
      if(idxcl > stopwin)
      {
        isstop = T
        r = data.frame(time = index(p[idx,]),enter=enter,out =idxcl,isstop = isstop,type = type , profit = enter - idxcl )
        records = rbind(records,r)
        break
      }
    }
  }
  if(isstop == F)
  {
    r = data.frame(time = index(p[idx,]),enter=enter,out =idxcl,isstop = isstop,type = type , profit = ifelse(type == 'long',(idxcl - enter),( enter - idxcl)) )
    records = rbind(records,r)
  }
}
