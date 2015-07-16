#长线系统 并不追求入场的精确性
require(quantmod)
require(TTR)
require('dygraphs')
path = "D:/data/dest"
#Sys.setenv(TZ="UTC")
files = dir(path)
files=c('SH600004.TXT')
f = files[1]
fname = file.path(path,f)
pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",index.column=1) 
colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
time(pricedata)=as.POSIXct(time(pricedata))
pricedata=as.xts(pricedata)

weekdata = to.weekly(pricedata)
#weekdata = weekdata['2000/']
weekdata$sma = na.omit(SMA(Cl(weekdata),n=30))
pweekdata = Cl(weekdata)

deltratio = na.omit(Delt(weekdata$sma) * 100)
cumsumDeltratio = na.omit(cumsum(deltratio))

deltratioSD = runSD(deltratio,n=20)

old.par = par(mfrow=c(2,2))
plot(Cl(weekdata))
plot(cumsumDeltratio)
plot(deltratio)
plot(deltratioSD)
par(old.par)

getratio = function(y)
{
  x = 1:length(y)
  f = lm(x~y)
  coff = f$coefficients[[2]]
  return(coff)
}
y = rollapply(cumsumDeltratio,width=25,FUN=getratio)

y = cumsumDeltratio[1:25]

t = as.character(index(y))
#尺度不一样 从1到2 和从100到200其实是不同的
merge(y,Cl(weekdata[t]))

x =seq(from=1,to=100, length.out=10)
y = 1:10
x1 = seq(from=500,to=600,length.out=10)

#使用累积增长率 滑动窗口
deltratioSlideLong = na.omit(runSum(deltratio,n=20))
deltratioSlideShort = na.omit(runSum(deltratio,n=3))
deltratioSlide = na.omit(merge(deltratioSlideLong,deltratioSlideShort))

m = na.omit(rollapply(deltratioSlide,width=20,FUN=getratio))
old.par = par(mfrow=c(2,2))

plot(pweekdata)
plot(deltratioSlideLong)
plot(deltratioSlideShort)
plot(na.omit(deltratioSlideShort-deltratioSlideLong))
par(old.par)

alldata =merge(pweekdata,deltratioSlide)
dygraph(pweekdata['2008-03-28/2008-11-28'])

ns = which(index(pweekdata)=='2013-01-04')
ne = ns - 25

pweekdata[ns:ne,]
deltratio[ns:ne,]
deltratioSlideLong[ns:ne,]
#猜想 long在 5 - -5 short 2 - -2之间可能是阶段划分的边界
