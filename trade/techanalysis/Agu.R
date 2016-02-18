require("quantmod")
zz=read.table("/Users/ruizhu/Desktop/stock/test.txt",header=TRUE,skip=0,sep="\t",as.is=F,fileEncoding="ISO-8859-1")
sed 's/^M//g' SZ002090.TXT | sed '1d;$d' >test.txt   
stockCode<-"000004.sz"
setSymbolLookup(code=list(name=stockCode,src="yahoo")) 
temp<-getSymbols("code",auto.assign=F)
history<-list(NULL)
history<-list(temp,history)
judgeWeek3<-function(x)
{
  d<-weekdays(as.Date(x))
  if(d=="Wednesday") TRUE else FALSE
}
i=sapply(rownames(dframe),judgeWeek3)
index=which(i)
we=df[index]
tu=df[index+1]
increase<-(as.vector(Cl(tu))-as.vector(Op(we)))/as.vector(Op(we))
sum(increase[is.finite(increase)])
