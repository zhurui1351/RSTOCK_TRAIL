#根据stageanalyse的代码整合在函数
rm(list=ls(all=T))
require(quantmod)
require(TTR)
require('dygraphs')
require('lubridate')
require('dplyr')

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/help')

#加入大盘阶段判断信息
shindex = readSHindex()
shindex_week = to.weekly(shindex)
shindex_week$sma30 = SMA(Cl(shindex_week),n=30)
shindex_week = na.omit(shindex_week)

shindex_week$stage = judegeStage(shindex_week$sma30)
shindex_week = na.omit(shindex_week)

#添加周平均成交 周大盘上升股比例交量等信息
shindex_week$meanVolume = apply.weekly(shindex[,'Volume'],mean)

#读入所有行业
lookups_hy = readallHy()

#读入行业代码
codeTable = readHycode()

#读入所有数据
lookups = readallstock(codeTable,shindex_week)
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
#shindex_week = shindex_week['2000/']
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