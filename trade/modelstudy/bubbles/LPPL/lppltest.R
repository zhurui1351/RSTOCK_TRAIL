#power law分布模拟图
require(quantmod)
x=seq(1,100,by=0.5)
y=x^(-1.2)
par(mfrow=c(1,3))
plot(x,y,type='l')
plot(x,log(y),type='l')
plot(log(x),log(y),type='l')
par(mfrow=c(1,1))

#
lp = function(t,tc,A,B,C,w,o){
  y = A+B*(tc-t)^2+C*(tc-t)^2*cos(w*log((tc-t)+0i) +o)
  return(y)
}

A=7.745
B=-0.0006
C=0.000174
w=7.4
o=1.67
tc=100
t=1:200
y = sapply(t,function(x){return(lp(x,tc,A,B,C,w,o))})

plot(1:100,y[1:100])
plot(101:200,y[101:200])

#critical point price
pt = function(t,tc,pc,k,B,b){
  return(pc - (k*B/b) *(tc-t)^b)
}
pc = 1002
tc = 500
t = 1: 500
k = 0.5
b=0.7
B = 2
y = sapply(t,function(x){return(pt(x,tc,pc,k,B,b))})
plot(t,y)

#Weibull distribution x>=0
wuibullfx = function(x,z,l)
{
  return(z * ((x^z-1)/l^z) * exp(-1*(x/l)^z))
}
x=seq(1,20,by=0.1)
y=sapply(x,function(t){return(wuibullfx(t,5.5,10))})
plot(x,y)

#common drawdown
x = runif(n=100,1,100)
x = c(2,1,2,1,2,1,1)
#find local max and min
localmax = findPeaks(x) - 1
localmin = findValleys(x) - 1
plot(x)

#find the flowing localmax
uppos = findInterval(localmax,localmin)
uppos = matrix(c(uppos,1:length(uppos)),ncol=2)
uppos = apply(uppos,FUN=function(x){if(x[1]!=0) return(list(start=localmin[x[1]],end=localmax[x[2]]))},MARGIN=1)
if(is.null(uppos[[1]])) uppos = uppos[2:length(uppos)]
#find the flowing localmin
downpos = findInterval(localmin,localmax)
downpos = matrix(c(1:length(downpos),downpos),ncol=2)
downpos = apply(downpos,FUN=function(x){if(x[2]!=0) return(list(start=localmax[x[1]],end=localmin[x[2]]))},MARGIN=1)
if(is.null(downpos[[1]])) downpos = downpos[2:length(downpos)]

for(i in 1:length(uppos))
{
  pos = c(uppos[[i]]$start,uppos[[i]]$end)
  lines(pos,x[pos])
}

for(i in 1:length(downpos))
{
  pos = c(downpos[[i]]$start,downpos[[i]]$end)
  lines(pos,x[pos])
}

start = 0
#filter by threhold
if(localmax[1]<localmin[1])
{
  start = localmax[1]
  localmax = localmax[-1]
}

imax = 1
imin = 1

#check every local min to judge if it is true local min
filteredmin = c()
ifmin = 1
filterdmax = c()
ifmax = 1
threshold = 1
while(i<=length(localmin))
{
  lmin = x[localmin[imin]]
  #the following local max
  lmax = x[localmin[imax]]
  
  if((lmax-lmin)>=threshold)
  {
    filteredmin[ifmin] = localmin[imin]
    filteredmin[ifmax] = localmin[imax]
    imin = imin + 1
    imax = imax + 1
    i = i + 1
  }
  else
  {
    
  }
   
}