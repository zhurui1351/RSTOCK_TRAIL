#根据stageanalyse的代码整合在函数
require(quantmod)
require(TTR)
require('dygraphs')
require('lubridate')
require('dplyr')

readSHindex = function()
{
  path = "D:/data/stock/index"
  #Sys.setenv(TZ="UTC")
  f='SH000001.TXT'
  fname = file.path(path,f)
  shindex = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",index.column=1) 
  colnames(shindex)<-c("Open","High","Low","Close","Volume","Amount")
  time(shindex)=as.POSIXct(time(shindex))
  shindex=as.xts(shindex)
  return(shindex)
}

readallstock = function()
{
  print(now())
  
  lookups = c()
  indexlookups =  1
  path = "D:/data/stock/dest"
  files = dir(path)
  rm(list=files)
  lookups = c()
  indexlookups =  1
  e = parent.env(environment())
  for(f in files)
  {
     #print(f)
    fname = file.path(path,f)
    pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",index.column=1) 
    if(nrow(pricedata) < 500){ next}
    colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
    time(pricedata)=as.POSIXct(time(pricedata))
    pricedata=as.xts(pricedata)
    pricedata = to.weekly(pricedata)
    colnames(pricedata)<-c("Open","High","Low","Close","Volume")
    pricedata$sma30 = SMA(pricedata,n=30)
    pricedata$volatile = (Cl(pricedata)-Op(pricedata))/Op(pricedata)
    
    pricedata = na.omit(pricedata)
    pricedata$stage = judegeStage(pricedata$sma30)
    pricedata = na.omit(pricedata)
    pricedata$meanVolume = apply.weekly(pricedata[,'Volume'],mean)
    
    assign(f,pricedata,envir=e)
    lookups[indexlookups] = f
    indexlookups = indexlookups + 1
  } 
  print(now())
  return(lookups)
}

judegeStage = function(smaData)
{
  getratio = function(y)
  {
    x = 1:length(y)
    f = lm(x~y)
    coff = f$coefficients[[2]]
    return(coff)
  }
  deltratio = na.omit(Delt(smaData) * 100)
  deltratioSlideLong = na.omit(runSum(deltratio,n=10))
  deltratioSlideShort = na.omit(runSum(deltratio,n=3))
  deltratioSlide = na.omit(merge(deltratioSlideLong,deltratioSlideShort))
  alldata = na.omit(merge(smaData,deltratioSlide))
  
  
  stage=apply(alldata,MARGIN=1,FUN=function(x){
    if(is.na(x['deltratioSlideLong']) || is.na(x['deltratioSlideShort']))
      return(-1)
    else
    {
      if(x['deltratioSlideLong']<= -2 && x['deltratioSlideShort'] <= -2) { return(4) }
      else if(x['deltratioSlideLong']>= 2 && x['deltratioSlideShort'] >= 2) {return(2)} 
      else {return(0)}
    }
    
  })
  names(stage) = ''
  stage = xts(stage,index(alldata))
  return(stage)
}
#加入大盘阶段判断信息
shindex = readSHindex()
shindex_week = to.weekly(shindex)
shindex_week$sma30 = SMA(Cl(shindex_week),n=30)
shindex_week = na.omit(shindex_week)

shindex_week$stage = judegeStage(shindex_week$sma30)
shindex_week = na.omit(shindex_week)

#添加周平均成交 周大盘上升股比例交量等信息
shindex_week$meanVolume = apply.weekly(shindex[,'Volume'],mean)


#读入所有股票
lookups = readallstock()
#形成列表
mg = mget(lookups)
mgl=lapply(mg,function(x){x$volatile})
mm = do.call("merge",args=mgl)

#计算上升比
uptorange = function(x)
{
  total = length(which(is.na(x) == F))
  i = length(which(x > 0 ))
  return(i / total)
}
uplist = apply(mm,FUN=uptorange,MARGIN=1)
names(uplist) = ''
uplist = xts(uplist,index(mm))
tempdata = apply.weekly(uplist,mean)
shindex_week = merge(shindex_week,tempdata)
shindex_week = na.omit(shindex_week)
colnames(shindex_week) = c('Open','Hign','Low','Close','Volume','sma30','stage','meanVolume','meanUpRatio')


#处理每个时间的筛选
shindex_week = shindex_week['2000/']
end = index(shindex_week)
#lapply(end,function(x,y){ 
 #                      return(list(x,y))},'d')

lapply(end[1],function(x){
  
  l = sapply(mg,function(p,date){
    s = Cl(p[date])
    return(s)
  }
    ,as.character(end[100]))
  l = Filter(function(x){!(length(x)==0)},l)
  print(l)
  return(NULL)
  
})