path = 'd:/ptable.csv'
data = read.csv(path)
subdata = subset(data,PointID=='3162241')

kdata = subdata[4:ncol(subdata)]
ksdata=apply(kdata,2,scale)

ksdata[is.na(ksdata)] =0

num_cluster = 3
f = kmeans(ksdata,num_cluster)

plot(ksdata, col = f$cluster)
points(f$centers, col = 1:num_cluster, pch = 8, cex = 2)
