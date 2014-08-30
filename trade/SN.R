require("quantmod")
judgeWeek3<-function(x)
{
  d<-weekdays(as.Date(x))
  if(d=="Wednesday") TRUE else FALSE
}

SN<-function(f,path)
{
  
  fname<-file.path(path,f)
 # fname="/Users/ruizhu/Desktop/stock/dest/SH600697.TXT"
 # print(fname)
  #try(stockdata<-read.table(fname,header=TRUE,skip=0,sep="\t",as.is=F,fileEncoding="ISO-8859-1"),TRUE)
 try(stockdata<-read.zoo(fname,header=TRUE, format = "%m/%d/%Y",sep="\t",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
# stockdata[[1]]<-format(as.Date(stockdata[[1]],"%m/%d/%Y"),"%Y-%m-%d")
  names(stockdata)<-c("Open","High","Low","Close","Volume","Amount")
 # stockdata<-read.zoo(stockdata)
  i<-sapply(index(stockdata),judgeWeek3)
  index<-which(i)
  
  
  if(length(i)==max(index))
  {
    index=index[-length(index)]
  }
  if(length(index)>1){
    we=stockdata[index,]
    try (tu<-stockdata[index+1,],TRUE)
    increase<-(as.vector(Op(tu))-as.vector(Op(we)))/as.vector(Op(we))
    increase<-increase[is.finite(increase)]
    increase<-increase[-which(increase>=0.1)]
    increase<-increase[-which(increase<=-0.1)]
    ratio<-sum(increase[is.finite(increase)])
    tmp<-NULL
    meanValue<-mean(increase)
    if(ratio>=1)
    {
      print(meanValue)
      tmp<-list(ratio,summary(increase))
      names(tmp)<-list(f,"summary")
     # history=c(history,tmp)
      print(fname)
     # print(ratio)
    }
  }
  tmp
}

history<-list()
path="/Users/ruizhu/Desktop/stock/dest"
files<-dir(path)
for(f in files)
{
  tmp<-try(SN(f,path),TRUE)
  history<-c(history,tmp)
}


