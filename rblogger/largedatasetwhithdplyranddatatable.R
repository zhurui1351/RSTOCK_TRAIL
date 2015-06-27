#http://freakonometrics.hypotheses.org/19645
#通过随机漫步的司机路线展示大数据处理
#1000个driver 200次 80个地方
rm(list=ls())
N_id=1000
N_tr=100
T_tr=40
set.seed(1)
N=rpois(N_id,N_tr)
N_traj=rpois(sum(N),T_tr)
#a driver Id., a Trajectory Id., and a location (latitude and longitude) at some specific dates (e.g. every 15 sec.). 
origin_lat=runif(N_id,-5,5)
origin_lon=runif(N_id,-5,5)
#产生随机漫步 二维的
lat=lon=Traj_Id=rep(NA,sum(N_traj))
Pers_Id=rep(NA,length(N_traj))
s=1
for(i in 1:N_id){Pers_Id[s:(s+N[i])]=i;s=s+N[i]}
s = 1
for(i in 1:length(N_traj)){
  lat[s:(s+N_traj[i])]=origin_lat[Pers_Id[i]]+ cumsum(c(0,rnorm(N_traj[i]-1,0,sd=.2)));
  lon[s:(s+N_traj[i])]=origin_lon[Pers_Id[i]]+cumsum(c(0,rnorm(N_traj[i]-1,0,sd=.2)));
  s=s+N_traj[i]}

#Driver Id., and the Trajectory Id
Pers_Id=rep(NA,sum(N_traj))
N0=cumsum(c(0,N))
s=1
for(i in 1:N_id){
  Pers_Id[s:(s+sum(N_traj[(N0[i]+1):N0[i+1]]))]=i
  s=s+sum(N_traj[(N0[i]+1):N0[i+1]])
}

s=1
for(i in 1:sum(N)){
  Traj_Id[s:(s+N_traj[i])]=i;s=s+N_traj[i]
}

df=data.frame(Pers_Id,Traj_Id,lat,lon)
#提高效率 使用data.table
library(dplyr)
ldf=data_frame(Pers_Id,Traj_Id,lat,lon)
library(data.table)
dt=data.table(Pers_Id,Traj_Id,lat,lon)
object.size(df)

#查找在某一结束点一定范围内的，起点信息
#用三种方式体现三个包的
system.time(n <- nrow(df))
system.time(df$first<-c(1,df$Traj_Id[2:n]!=
                          df$Traj_Id[1:(n-1)]))
system.time(df$last<-c(df$Traj_Id[2:n]!=
                         df$Traj_Id[1:(n-1)],1))
object.size(df)
lat_0=0
lon_0=0
#system.time(df$test <-(((lat_0-df$lat)^2+(lon_0-df$lon)^2 <=1)&(df$last==1)))
df$test <-((lat_0-df$lat)^2+(lon_0-df$lon)^2)<=1 & (df$last==1)
df$first <- NULL
df$last <- NULL
object.size(df)
system.time(df$test)
system.time(list_Traj <- unique(df$Traj_Id[df$test==TRUE]))
system.time(base <- df[(df$Traj_Id %in% list_Traj)&(c(1,df$Traj_Id[2:n]!=df$Traj_Id[1:(n-1)])==1),c("lat","lon")])
head(base)
X <- base[,c("lon","lat")]
library(KernSmooth)
kde2d <- bkde2D(X, bandwidth=c(bw.ucv(X[,1]),bw.ucv(X[,2])),gridsize = c(251L, 251L))
image(x=kde2d$x1, y=kde2d$x2,z=kde2d$fhat,col=
        rev(heat.colors(100)))
contour(x=kde2d$x1, y=kde2d$x2,z=kde2d$fhat, add=TRUE)

system.time( depart <- dt[J(unique(Traj_Id)), mult = "first"])
system.time( setkey(dt,Traj_Id) )
system.time(depart <-dt[!duplicated(Traj_Id)])

lat_0=0
lon_0=0
system.time( arrivee[,dist:=(lat-lat_0)^2+(lon-lon_0)^2] )
system.time( fin <- subset(arrivee,dist <= 1) )