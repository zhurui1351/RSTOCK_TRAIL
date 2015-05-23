#测试找到上市以来每天涨跌在一定幅度的股票
require(quantmod)
require(TTR)
library(blotter)

path = "D:/data/dest"
#Sys.setenv(TZ="UTC")
files = dir(path)
#files=c('SH603606.TXT')
#f = files[200]

rm(list=files)
lookups = c()
indexlookups =  1
#read all the data
for(f in files)
{
  print(f)
  fname = file.path(path,f)
  pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",index.column=1) 
  colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
  time(pricedata)=as.POSIXct(time(pricedata))
  pricedata=as.xts(pricedata)
  pricedata$volatile = (Cl(pricedata)-Op(pricedata))/Op(pricedata)
  if(nrow(pricedata) < 500){ next}
  assign(f,pricedata)
  lookups[indexlookups] = f
  indexlookups = indexlookups + 1
}
#循环找到每日跌幅涨幅一定的数据
days = index(SH600000.txt)
upstoplist = list()
downstoplist = list()
idays = 1
for(day in length(days):length(days))
{
  print(days[day])
  dayup = list()
  daydown = list()
  iupday = 1
  idownday = 1
  for(symbol in 1:length(lookups))
  {
    p = get(lookups[symbol])[days[day]] 
    v = as.numeric(p$volatile)
    if(nrow(p) == 0 || is.nan(p$volatile) == T) {next}
    if(v >= 0.08)
    {
      dayup[[iupday]] = lookups[symbol]
      iupday = iupday + 1
    }
    if(v <= -0.08)
    {
      daydown[[idownday]] = lookups[symbol]
      idownday = idownday + 1
    }
    
  }
  upstoplist[[idays]] = dayup
  downstoplist[[idays]] = daydown
  idays = idays+1
}
names(upstoplist) = days
names(downstoplist) = days

#使用matrix处理，效率提升很快
mg = mget(lookups)
mg=lapply(mg,function(x){x$volatile})
#mm=merge(p,p1)
mm = do.call("merge",args=mg)
ma=as.matrix(mm)

uptorange = function(x)
{
  i = which(x>=0.08)
  if(length(i) == 0) return(NULL)
  return(lookups[i])
}
downtorange = function(x)
{
  i = which(x<=-0.08)
  if(length(i) == 0) return(NULL)
  return(lookups[i])
}
relup = apply(ma,FUN=test,MARGIN=1)
reldown = apply(ma,FUN=test,MARGIN=1)
