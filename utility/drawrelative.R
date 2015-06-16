drawRelative<-function(x1,x2,timespan='2010/2011')
{
  x1 = x1[timespan]
  x2 = x2[timespan]
  xlim_start = min(min(time(x1)),min(time(x2)))
  xlim_end = max(max(time(x1)),max(time(x2)))
  ylim = max(max(Cl(x1)),max(Cl(x2)))+5
  plot(time(x1),Cl(x1),type='l',col='red',xlim=c(xlim_start,xlim_end),ylim=c(0,ylim)
       ,xlab='time',ylab='price')
  lines(time(x2),Cl(x2),col='green')
  
  x3 = merge(Cl(x1),Cl(x2))
  x3=na.omit(x3)
  rel =  cor(as.numeric(x3[,1]),as.numeric(x3[,2]))
  legend("topright",  title="Drug Type", c("A","B"),
         lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"))
}