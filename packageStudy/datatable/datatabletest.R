DF = data.frame(x=c("b","b","b","a","a"),v=rnorm(5))
DT = data.table(x=c("b","b","b","a","a"),v=rnorm(5))
CARS = data.table(cars)
head(CARS)
#列出所有data.table对象
tables()
sapply(DT,class)
#选择第二行
DT[2,]
#选择x列为'b'的行
DT[x=='b',]
#建立key为x列
setkey(DT,x)
#课件DT按x列被重新排序
DT
#所以x列为‘b’的行
DT['b',]
DT['b',]
DT["b",mult="first"]
DT["b",mult="last"]

grpsize = ceiling(1e7/26^2)
#比较和data.frame向量扫描与二项扫描的不同
tt=system.time( DF <- data.frame(
   x=rep(LETTERS,each=26*grpsize),
   y=rep(letters,each=grpsize),
   v=runif(grpsize*26^2),
   stringsAsFactors=FALSE)
   )

head(DF,3)
tail(DF,3)
dim(DF)

#data.frame 使用向量扫描方式进行查询
tt=system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",])
tt
head(ans1,3)
dim(ans1)
#使用data.table试试
DT = as.data.table(DF)
#x,y列作为key
system.time(setkey(DT,x,y))
#二分搜索 速度快了很多 近百倍,一个是O(n) 一个是O(logn)
ss=system.time(ans2 <- DT[list("R","h")])
head(ans2,3)
dim(ans2)
identical(ans1$v, ans2$v)
#在data.table中使用向量扫描，速度比较慢
system.time(ans1 <- DT[x=="R" & y=="h",])
system.time(ans2 <- DF[DF$x=="R" & DF$y=="h",])
mapply(identical,ans1,ans2)
#等价
identical( DT[list("R","h"),],DT[.("R","h"),])

#dt[i,j,by...] 考虑参数j
DT[,sum(v)]
#by参数用于分组
DT[,sum(v),by=x]
#使用by的方式比传统方式速度更快
ttt=system.time(tt <- tapply(DT$v,DT$x,sum)); ttt
sss=system.time(ss <- DT[,sum(v),by=x]); sss
identical(as.vector(tt), ss$V1)
