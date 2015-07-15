require(quantmod)
require(TTR)

path = "D:/data/dest"
#Sys.setenv(TZ="UTC")
files = dir(path)
files=c('SH000001.TXT')
f = files[1]
fname = file.path(path,f)
pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",index.column=1) 
colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
time(pricedata)=as.POSIXct(time(pricedata))
pricedata=as.xts(pricedata)

weekdata = to.weekly(pricedata)
weekdata = weekdata['2000/']
weekdata$sma = na.omit(SMA(Cl(weekdata),n=30))
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
  x = 1:length(x)
  f = lm(x~y)
  coff = f$coefficients[[2]]
  return(coff)
}
y = rollapply(cumsumDeltratio,width=25,FUN=getratio)

y = cumsumDeltratio[1:25]

t = as.character(index(y))
#尺度不一样 从1到2 和从100到200其实是不同的
merge(y,Cl(weekdata[t]))

