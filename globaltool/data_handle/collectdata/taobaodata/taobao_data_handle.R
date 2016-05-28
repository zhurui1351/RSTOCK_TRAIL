sourceDir('D:/Rcode/code/RSTOCK_TRAIL/globaltool/date_handle',encoding='utf8')

getbasetime_night_crossday = function()
{
  start = as.POSIXct('2000-01-01 21:01:00')
  mytimes = c(start)
  for(i in 1:329)
  {
    temp = start + i*60 
    mytimes = c(mytimes,temp)
  }
  return(substring(as.character(mytimes),12))
}

getbasetime_night = function()
{
  start = as.POSIXct('2000-01-01 21:01:00')
  mytimes = c(start)
  for(i in 1:149)
  {
    temp = start + i*60 
    mytimes = c(mytimes,temp)
  }
  return(substring(as.character(mytimes),12))
}


getbasetime_day = function()
{
  mstart = as.POSIXct('2000-01-01 09:01:00')
  mstart1 = as.POSIXct('2000-01-01 10:31:00')
  astart = as.POSIXct('2000-01-01 13:31:00')
  mytimes = c(mstart)
  
  for(i in 1:74)
  {
    temp = mstart + i * 60
    mytimes = c(mytimes,temp)
  }
  
  mytimes = c(mytimes,mstart1)
  for(i in 1:59)
  {
    temp = mstart1 + i * 60
    mytimes = c(mytimes,temp)
  }
  
  mytimes = c(mytimes,astart)
  for(i in 1:89)
  {
    temp = astart + i * 60
    mytimes = c(mytimes,temp)
  }
  return(substring(as.character(mytimes),12))
  
}


collectdatafromtaobao = function(pattern = 'DLAMI*')
{
  path = 'D:/BaiduYunDownload/data'
  files = list.files(
    path,pattern = pattern ,recursive = T,ignore.case = T,full.names = T
  )
  #日期，时间，开盘，最高，最低，收盘 ，成交量，持仓量
  f = files[1]
  da = read.csv(f,header = F,sep = ",")
  colnames(da) = c('Date','Time','Open','High','Low','Close','Vol','Oi')
  da$datetime = paste(da[,1],da[,2])
  da$Vol = 0
  da = xts(da[,3:8],order.by = as.POSIXct(da$datetime))
  pricedata = da
  
  for (f in files[2:length(files)])
  {
    da = read.csv(f,header = F,sep = ",")
    colnames(da) = c('Date','Time','Open','High','Low','Close','Vol','Oi')
    da$datetime = paste(da[,1],da[,2])
    da = xts(da[,3:8],order.by = as.POSIXct(da$datetime))
    
    pricedata = rbind(pricedata,da)
  }
  
  newpricedata = NULL
  #去除间隔 白天交易
  basedates = as.character(unique(as.Date(index(pricedata))))
  # basetime = substring(as.character(unique(index(pricedata['2003-08-01']))),12)
  # basenum = nrow(pricedata['2003-08-01'])
  basetime = getbasetime_day()
  basenum = length(basetime)
  lackdays = c()
  for (day in basedates)
  {
    daystart = paste(day,'09:01:00')
    dayend = paste(day,'15:00:00')
    dayperd = paste(daystart,dayend,sep='/')
    p = pricedata[dayperd]
    nump = nrow(p)
    #k线有缺失
    if (basenum != nump)
    {
      lackdays = c(lackdays,day)
      dayprice = pricedata[dayperd]
      daytime = substring(index(dayprice),12)
      #默认排好序
      missingtime = setdiff(basetime,daytime)
      for (mt in missingtime)
      {
        i = which(basetime == mt)
        while (!is.element(basetime[i],daytime) && i > 0 )
        {
          i = i - 1
        }
        if(i == 0 )
        {
          j = length(daytime)
        }
        else
        {
          j = which(daytime == basetime[i])
        }
        missp = dayprice[j]
        index(missp) = as.POSIXct(paste(day,mt))
        missp$High = missp$Open
        missp$Low = missp$Open
        missp$High = missp$Open
        missp$Close = missp$Open
        missp$Vol = 0  
        p = rbind(p,missp)
      }
    }
    newpricedata = rbind(newpricedata,p)
  
  }
  #2014年开始推出夜盘 当日21:00-次日2:30 2015年5月8日期调整夜盘时间为当日21:00 - 23:30
  period1 = as.character(index(pricedata['201401/20150507']))
  nighttime1 =substr(period1,12,13) 
  #寻找有夜盘的第一个交易日
  i = match('21',nighttime1)
  period1start = as.character(as.Date(period1[i]))
  period1 = paste(period1start,'2015-05-07',sep='/')
  period1dates = as.character(unique(as.Date(index(pricedata[period1]))))
  
  lackdays = c()
  
  basetime = getbasetime_night_crossday()
  basenum = length(basetime)
  for (day in period1dates)
  {
    nday = as.character(as.Date(day) + 1)
    starttime = paste(day,'21:00:00')
    endtoday = paste(day,'23:59:00')
    
    todayperd = paste(starttime,endtoday,sep='/')
    #周末没有夜晚的数据
    if(nrow(pricedata[todayperd]) == 0)
      next
    
    endtime = paste(nday,'02:30:00')
    perd = paste(starttime,endtime,sep='/')
    p = pricedata[perd]
    nump = nrow(p)
    #k线有缺失
    if (basenum != nump)
    {
      lackdays = c(lackdays,day)
      dayprice = pricedata[perd]
      daytime = substring(index(dayprice),12)
      #默认排好序
      missingtime = setdiff(basetime,daytime)
      for (mt in missingtime)
      {
        i = which(basetime == mt)
        while (!is.element(basetime[i],daytime) && i > 0 )
        {
          i = i - 1
        }
        if(i == 0 )
        {
          j = length(daytime)
        }else
        {
          j = which(daytime == basetime[i])
        }
        missp = dayprice[j]
        
        whichday = substring(as.character(index(missp)),1,10)
        index(missp) = as.POSIXct(paste(whichday,mt))
        missp$High = missp$Open
        missp$Low = missp$Open
        missp$High = missp$Open
        missp$Close = missp$Open
        missp$Vol = 0
        p = rbind(p,missp)
      }
    }
    newpricedata = rbind(newpricedata,p)
  }
  #2015年5月8日期调整夜盘时间为当日21:00 - 23:30
  period2start = '2015-05-08 09:00:00'
  perid2 = paste(period2start,'',sep='/')
  period2dates = as.character(unique(as.Date(index(pricedata[perid2]))))
  
  basetime = getbasetime_night()
  basenum = length(basetime)
  
  lackdays = c()
  
  for (day in period2dates)
  {
    starttime = paste(day,'21:00:00')
    endtime = paste(day,'23:30:00')
    perd = paste(starttime,endtime,sep='/')
    p = pricedata[perd]
    nump = nrow(p)
    #可能没有夜盘
    if(nump == 0) next
    #k线有缺失
    if (basenum != nump)
    {
      lackdays = c(lackdays,day)
      dayprice = pricedata[perd]
      daytime = substring(index(dayprice),12)
      #默认排好序
      missingtime = setdiff(basetime,daytime)
      for (mt in missingtime)
      {
        i = which(basetime == mt)
        while (!is.element(basetime[i],daytime) && i > 0 )
        {
          i = i - 1
        }
        if(i == 0 )
        {
          j = length(daytime)
        }else
        {
          j = which(daytime == basetime[i])
        }
        missp = dayprice[j]
        index(missp) = as.POSIXct(paste(day,mt))
        missp$High = missp$Open
        missp$Low = missp$Open
        missp$High = missp$Open
        missp$Close = missp$Open
        missp$Vol = 0
        p = rbind(p,missp)
      }
    }
    newpricedata = rbind(newpricedata,p)
  }
  return(newpricedata)
}



