record= logger$record
if(nrow(record)%%2 != 0)
{
  record = record[1:(nrow(record)-1),]
}
index = 1:nrow(record)
enter = record[which(index%%2==1),]
exit = record[which(index%%2==0),]
records = cbind(enter,exit)
colnames(records) = c('enterdate','enterprice','entertype','exitdate','exitprice','exittype')
net = records$exitprice - records$enterprice

point = records$net
totalpoint = sum(point)
pratio = length(point[point>0]) / length(point)
nratio = length(point[point<0]) / length(point)
totalcount = length(point)

records=read.zoo(records,header=T,index.column = 1,format="%Y-%m-%d %H:%M:%S", tz="")
records = as.xts(records)
records$net = net

p = performanceInfo(-records$net)
