require("quantstrat")
computeTrend<-function(f,path,n=30) # data is a zoo object ,data has rownames c("Open","High","Low","Close","Volume","Amount")
{
  fname<-file.path(path,f)
 # print(fname)
  try(stockdata<-read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
  names(stockdata)<-c("Open","High","Low","Close","Volume","Amount")
  weekdata<-to.weekly(stockdata)
  colnames(weekdata)<-c("Open", "High", "Low", "Close","Volume")
  sma=SMA(Cl(weekdata),n=n)
  recent=last(sma,5)
  myframe=data.frame(recent,index=1:length(recent))
  mylm=lm(recent~index,myframe)
  coff<-mylm$coefficients["index"]
  tmp<-NULL
  if(coff>0){
    tmp<-list(coff,summary(coff))
    names(tmp)<-list(f,"summary")
  }
  tmp
 # x<-Cl(weekdata)
  #x<-data.frame(x,index(x))
  #colnames(x)<-c("Close","Date")
  #for (i in seq(0.01, 1, length = 100)) {
  #  lines(lowess(x$Close, x$Date, f = i), col = gray(i),
   #       lwd = 1.5)
  #  Sys.sleep(0.15)
#  }
}
history<-list()
path="/Users/ruizhu/Desktop/stock/dest"
files<-dir(path)
for(f in files)
{
  tmp<-try(computeTrend(f,path),TRUE)
  try(if(!is.null(tmp) && names(tmp) != ""){
    history<-c(history,tmp)
  },TRUE)
}
