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
  r = data.frame(open = open,end1 = end1,end2=end2,sign1 = sign(end1 - open),sign2 = sign(end2 - open))
  records = rbind(records,r)
}


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
