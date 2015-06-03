#分析大幅涨跌后一段时间内的情况
require(quantmod)
require(TTR)
library(blotter)

path = "D:/data/dest"
#Sys.setenv(TZ="UTC")
files = dir(path)
#files=c('SH000001.TXT')
f = files[1]
resultlist=list()
resultlistindex = 1
print(files)
for(f in files)
{
  print(f)
  fname = file.path(path,f)
  pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep="\t",index.column=1) 
  colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
  time(pricedata)=as.POSIXct(time(pricedata))
  pricedata=as.xts(pricedata)
  pricedata$volatile = (Cl(pricedata)-Op(pricedata))/Op(pricedata)
  if(nrow(pricedata) < 500){ next}
  
  
  result = c()
  result_index = 1
  dates = time(pricedata)
  for(i in 1 : length(dates))
  {
    if(!is.nan(pricedata[i,]$volatile) && as.numeric(pricedata[i,]$volatile) < -0.08)
    {
     # print(pricedata[i:(i+2),])
     # print("next")
      #2天后的情况
      if((i+2) > nrow(pricedata)){next}
      cn = as.numeric(Op(pricedata[(i+2),]))
      cc = as.numeric(Op(pricedata[(i+1),]))
      inc =(cn-cc) / cc
      result[result_index] = inc
      result_index = result_index + 1
    }
  }
  resultlist[[resultlistindex]] = result
  resultlistindex = resultlistindex + 1
}

r = sapply(resultlist,function(m){length(m[m>0])/length(m)})
r1 = na.omit(r)
length(r1[r1>0.55]) / length(r1)
save(resultlist,file='resultlist.Rdata')

