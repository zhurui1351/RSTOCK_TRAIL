rm(list=ls(all=T))
require(quantmod)
require(TTR)
require('dygraphs')
require('lubridate')
require('dplyr')
require('dygraphs')
require('e1071')

source('D:/Rcode/code/RSTOCK_TRAIL/globaltool/generaltool.R')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/help',encoding='utf8')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/bullandbear/help',encoding='utf8')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/macrodata/',encoding='utf8')
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/macrodata/help',encoding='utf8')


#shindex = getSymbols('000001.SS',auto.assign = F,from='1990-01-01')

#shindex = getSymbols('^DJI',auto.assign = F,from='1970-01-01')

#shindex = adjustOHLC(shindex,use.Adjusted = T)
#colnames(shindex) = gsub('DJI.','',colnames(shindex))

shindex = readSHindex()
shindex_w = to.weekly(shindex)
colnames(shindex_w) = gsub('shindex.','',colnames(shindex_w),fixed=T)
shindex_m = to.monthly(shindex)
colnames(shindex_m) = gsub('shindex.','',colnames(shindex_m),fixed=T)

myindex = shindex_w

bulllist  = find_bull(upratio = 1,downratio = -0.3,shindex = myindex)
enterpoints = find_upstage_enter(myindex,startupratio = 0.3,upratio = 1,breakdownratio = -0.3,downratio = -0.2)
#enterpoints = find_upstage_enter(myindex,startupratio = 0.1,upratio = 0.3,breakdownratio = -0.1,downratio = -0.1)

records = enterpoints[[1]]
curstatuts = enterpoints[[2]]

bearlist = find_bear(upratio = 0.2,downratio = -0.4,shindex = myindex)


#看图分析bullist
for(i in 1:(length(bulllist)-1))
{
  bullfrom = as.character(bulllist[[i]]['from'])
  bullto = as.character(bulllist[[i]]['to'])
  bullend = as.character(bulllist[[i]]['end'])
  statdate = paste(bullfrom,bullto,sep='/')
 # print(statdate)
#  print(sd(Delt(myindex[statdate]$Close),na.rm=T))
 # print(mean(Delt(myindex[statdate]$Close),na.rm=T))
  
  str = paste('从',bullfrom ,'的最低',myindex[bullfrom]$Low,'到',bullto,"的最高",myindex[bullto]$High
              ,'到',bullto,'的最低',myindex[bullend]$Low,'结束',sep='')
  print(str)
  plotdata = cbind(Cl(myindex[statdate]),myindex[statdate]$sma)
 # chartSeries(OHLC(myindex),TA=c(addSMA(n=12,col = "green"),addSMA(n=4,col = "red")),subset=paste(bullfrom,bullend,sep='::'))

#  addTA(Cl(myindex[statdate]), on=1, col='yellow')
  
 # tmp = scan()
}
#人工看图分析
for(i in 1:length(bearlist))
{
  bearfrom = bearlist[[i]]['from']
  bearto = bearlist[[i]]['to']
  bearend = bearlist[[i]]['end']
  statdate = paste(bearfrom,bearto,sep='/')
  print(statdate)
  print(sd(Delt(myindex[statdate]$Close),na.rm=T))
  print(mean(Delt(myindex[statdate]$Close),na.rm=T))
  plotdata = cbind(Cl(myindex[statdate]),myindex[statdate]$sma)
  chartSeries(myindex,TA=c(addSMA(n=100,col = "green"),addSMA(n=30,col = "red")),subset=paste(bearfrom,bearend,sep='::'))
  tmp = scan()
}



####basic stat
myindex = shindex_w
myindex$sma = SMA(myindex$Close,n = 12)
myindex$preclose = lag(myindex$Close,1)
myindex$presma = lag(myindex$sma,1)
cnt = ifelse(myindex$preclose < myindex$presma & myindex$Close >myindex$sma,1,0 )
sum(as.numeric(cnt),na.rm = T)
breakout_sma = cnt[which(cnt == 1),]

bulist = bulllist[1:(length(bulllist)-1)]
from_bull = sapply(bulist, function(x){return(as.character(x['from']))})

sapply(bulist, function(x){as.numeric(as.Date(x['to']) - as.Date(x['from'])) })

#人工看图
for(i in 1: length(bulist))
{
  i = i + 1
  l = bulist[[i]]
  tmpdate = paste(l['from'],l['to'],sep='/')
  tmpdate1 = paste(l['from'],l['to'],sep='::')
  
  i1 = which(index(myindex) == l['from'])
  i2 = which(index(myindex) == l['to'])
  
  revindex = as.data.frame(myindex)[i2:i1,]
  
  print(find_bull(upratio = 0.3,downratio = -0.1,myindex[tmpdate])) 
  dygraph(myindex[tmpdate]$Close)
  tmp = scan()
}

# 主观宏观数据分析，观测数据并在其他地方标注
macrodata = readmacrodata()

for(i in 1 : nrow(records))
{

  indicator = interestrate_1y_f_d
  start = records[i,'start']
  #前半年的时间
  prestart = start - months(6)
  if(is.na(prestart))
  {
    prestart = start - days(1) - months(6)
    prestart = prestart + days(1)
  }
  afterstart = start + months(1)
  perd = paste(prestart,start,sep='/')
  print(indicator[perd])
  print(i)
  tmp = scan()
  
}

#采用量化分段，对进场点附近的宏观数据进行分析
getperd = function(start)
{
  prestart = start - months(6)
  if(is.na(prestart))
  {
    prestart = start - days(1) - months(6)
    prestart = prestart + days(1)
  }
  afterstart = start + months(1)
  perd = paste(prestart,start,sep='/')
  return(perd)
}

# 对宏观数据进行分段处理
modeldata = records
varset = c('interestrate_1y_f_d','exchg_usd_rmb_m','cpi_m2m_m','cci_m','emp_m2m_q')
startpoints = records[,'start']
allperds = sapply(startpoints, getperd)

for(var in varset)
{
  tmpnames = colnames(modeldata)
  indicator = get(var)
  func = getFunction(paste(var,'_seg',sep=''))
  segflag = sapply(allperds, func,indicator)
  modeldata = cbind(modeldata,segflag)
  colnames(modeldata) = c(tmpnames,var)
}



#贝叶斯分析
#探索主观分析所形成的文件
#path = 'd:/data/result.csv'
#result = read.csv(path)
#result[result==''] = NA
#flag = records[,"flag"]
#records_model = cbind(flag,result)
flag = records[,"flag"]
records_model = modeldata[,c('flag',varset)]
#varset = c('interestrate_1y_f_d','exchg_usd_rmb_m','cpi_m2m_m','cci_m','emp_m2m_q')

varset = c('interestrate_1y_f_d','exchg_usd_rmb_m','cpi_m2m_m','cci_m','emp_m2m_q')

vars = paste(varset,collapse = '+')
f = formula(paste('flag ~ ',vars))

model = naiveBayes(f,data=records_model[,c('flag',varset)],na.action = na.pass,laplace = 1)

pr = predict(model,records_model[,varset],type='raw')
cbind(as.data.frame(pr),as.character(flag))
pr1 = predict(model,records_model[,varset])
table(pr1,flag)


#latent
datasubset = records_model

ecode=function(x){
  
  tmp = sapply(x,function(x){
    if(is.na(x))
      return(NA)
    else if(x == '下降')
      return('0')
    else if (x == '不变')
      return('1')
    else if (x == '上涨')
      return('2')
    else if (x == '升值')
      return('2')
    else if (x == '贬值')
      return('1')
    else if(x == 'succ')
      return('1')
    else if(x == 'failup')
      return('0')
    else return(as.character(x))
  })
  return(tmp)
}
varset = c('interestrate_1y_f_d','exchg_usd_rmb_m','cpi_m2m_m')

data_train = as.data.frame(mapply(ecode,datasubset),row.names=rownames(datasubset))
HNBstudy(na.omit(data_train),varset,'flag')
