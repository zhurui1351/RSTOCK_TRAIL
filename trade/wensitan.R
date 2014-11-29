require("quantstrat")
#week up day down
trendlongUpShortDown<-function(stockdata,n=30,m=5) # data is a zoo object ,data has rownames c("Open","High","Low","Close","Volume","Amount")
{
  tmp<-NULL
  if(!is.zoo(stockdata))
  {
    return(tmp)
  }
  weekdata<-to.weekly(stockdata)
  colnames(weekdata)<-c("Open", "High", "Low", "Close","Volume")
  sma=SMA(Cl(weekdata),n=n)
  recent=last(sma,m)
  myframe=data.frame(recent,index=1:length(recent))
  mylm=lm(recent~index,myframe)
  coff<-mylm$coefficients["index"]
  
  recentPrice=last(Cl(stockdata),5)
  priceFrame=data.frame(recentPrice,index=1:length(recentPrice))
  priceLm=lm(recentPrice~index,priceFrame)
  shortTrend=priceLm$coefficients["index"]
 
  if(coff>0.1 && last(Cl(stockdata)) < 15 && last(Cl(stockdata)) > 10 && shortTrend < 0){
    tmp<-list(c(coff,shortTrend))
    print("h")
    names(tmp)<-c(f)
  }
   tmp
}

#last n week move stablely between raiton a and b, recent m week begin go up and ration greater than c
beginbreakup<-function(stockdata,period=30,n=10,m=3) # data is a zoo object ,data has rownames c("Open","High","Low","Close","Volume","Amount")
{
  tmp<-NULL
  if(!is.zoo(stockdata))
  {
    return(tmp)
  }
  weekdata<-to.weekly(stockdata)
  colnames(weekdata)<-c("Open", "High", "Low", "Close","Volume")
  number=length(index(weekdata))
  sma=SMA(Cl(weekdata),n=period)
  
  lastNweek=sma[(number-n-m):(number-m)]
  
  myframe=data.frame(recent=lastNweek,index=1:length(lastNweek))
  mylm=lm(recent~index,myframe)
  coff<-mylm$coefficients["index"]
  
  recentMweek=sma[(number-m):number]
  priceFrame=data.frame(recentPrice=recentMweek,index=1:length(recentMweek))
  priceLm=lm(recentPrice~index,priceFrame)
  shortTrend=priceLm$coefficients["index"]
  
  if(coff>-0.1 && coff < 0.1 &&  shortTrend > 0.2){
    tmp<-list(c(coff,shortTrend))
    print("h")
    names(tmp)<-c(f)
  }
  tmp
}

#
history<-list()
#path="/Users/ruizhu/Desktop/stock/dest"
path="D:/stock/dest"
files<-dir(path)
for(f in files)
{
  fname<-file.path(path,f)
  #print(fname)
  try(stockdata<-read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
  names(stockdata)<-c("Open","High","Low","Close","Volume","Amount")
  #tmp<-try(trendlongUpShortDown(stockdata),TRUE)
  tmp<-try(beginbreakup(stockdata),TRUE)
  try(if(!is.null(tmp) && names(tmp) != ""){
    history<-c(history,tmp)
  },TRUE)
}
