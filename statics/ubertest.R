library(dplyr)
library(reshape2)
require(quantmod)
Sys.setlocale("LC_TIME", "C")
tb= read.csv('c:/uber.csv',head=T,sep=',')
head(tb)
tb[,5]
max(tb[,5])
tb[281,]
tb[,which(tb[,6]==max(tb[,6]))]

pretime = tb[1,1]
for( i in 1:nrow(tb))
{
   time = tb[i,1]
   if(time =="")
   {
     tb[i,1] = pretime
   }
   else
   {
     pretime = tb[i,1]
   }
}
tb$date = strptime(paste(tb$Date,tb[,2]),format='%d-%b-%y %H')
tb$date = as.POSIXct(tb$date,tz="")
complete=aggregate(tb[,5],by=list(tb$Date),FUN=sum)
max(complete$x)
request = complete=aggregate(tb[,6],by=list(tb[,2]),FUN=sum)


sum(tb[,5])/sum(tb[,7])

tt= zoo(tb,tb$date)
tt=xts(tt)
requestdiff = complete=aggregate(tb[,6]-tb[,7],by=list(tb[,2]),FUN=sum)

bonus= read.csv('c:/uber2.csv',head=T,sep=',')
bonus$Accept.Rate = as.numeric(gsub('%','',bonus$Accept.Rate))
bonus$Rating = as.numeric(bonus$Rating)
