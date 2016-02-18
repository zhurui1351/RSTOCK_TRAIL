findud<-function(v)
{
  vud<-v[-1]-v[-length(v)] #diff(v)
  return(ifelse(vud>0,1,-1)) #sign(vud)
}

udcorr<-function(x,y)
{
  ud<-lapply(list(x,y),findud)
  return(mean(ud[[1]]==ud[[2]]))
}

findwords<-funciton(tf)
{
  txt<-scan(tf,"")
  wl<-list()
  for(i in 1:length(txt))
  {
    wrd<-txt[i]
    wl[[wrd]]<-c(wl[[wrd]],i)
  }
  return wl
}

##debug test
findruns<- function(x,k)
{
  n<-length(x)
  runs<- NULL
  for(i in i:(n-k))
  {
    if(all(x[i:i+k-1])) runs<-c(runs,i)
  }
  return(runs)
}