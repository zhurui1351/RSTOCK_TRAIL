rm(list=ls(all=T))
require('quantmod')
require('TTR')
require('dygraphs')
require('lubridate')
require('dplyr')
require('data.table')
require('e1071')
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

#引入辅助函数
sourceDir('D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/help')

#读入数据
getSymbols("^N225",from="1900-01-01")
pricedata = adjustOHLC(N225,use.Adjusted = T)
#load('GSPC.Rdata')
colnames(pricedata) = gsub('N225.','',colnames(pricedata))
#pricedata = readSHindex()
pricedata = na.omit(pricedata)
#处理待预测数据，用lead提前一天，把头天的涨跌加入预测变量
cl = Cl(pricedata) - Op(pricedata)
clflag = ifelse(cl < 0 ,'down','up')
leadclflag = shift(clflag,n=1,type='lead')
analysedata = merge(leadclflag,clflag)

start = head(index(analysedata),1)
end = tail(index(analysedata),1)

testdate = substr(as.character(index(to.yearly(pricedata['1994/2014']))),1,4)
#starttraindate = as.character(1995)

#计算指标更新缓存 #c(4,6,11,16)
# 回测期间测试 4表示用前4年到前年的共计3年数据进行测试

longtotest = 3
for(y in testdate)
{
  print(y)
  starttraindate = as.character(as.numeric(y) - longtotest)
  endtraindate = as.character(as.numeric(y) - 1)
  analysedata_train = as.data.frame(analysedata[paste(starttraindate,endtraindate,sep='/')])
  #最后一期的数据不参与建模，比如20141231那天是无法知道2015第一个交易日的涨跌
  analysedata_train = analysedata_train[1:(nrow(analysedata_train) - 1),]
  
  starttrain = head(rownames(analysedata_train),1)
  endtrain =  tail(rownames(analysedata_train),1)
  #根据测试期间的参数 更新整个数据 比如测试期间最优sma参数是 3 10 则用该参数重算sma信号
  temp_analysisdata = getBsoptimsignal(pricedata,analysedata,starttrain,endtrain,start,end)
  cachename = paste('analysedata',y,longtotest,sep='_')
  #保存为缓存变量
  assign(cachename,temp_analysisdata)
}

varset = c('Close','smasignal','ccisignal','rsisignal','macdsignal','adxsignal','mfisignal','bbandssignal','rocsignal',
           'sarsignal','wprsignal','kdjsignal','tdisignal','kstsignal','chkADsignal','obvsignal','cmosignal',
           'cmfsignal','trixsignal','willimadsignal','emvsignal' )
#过滤指标
for(y in testdate)
{
  cachename = paste('analysedata',y,longtotest,sep='_')
  tempdata = get(cachename)
  isgetallvar = varset %in% colnames(tempdata)
  notexistindex = which(!isgetallvar)
  if(length(notexistindex) > 0)
  {
    varset = varset[-notexistindex]
    
  }
}

print(varset)
#指标组合
#指标集合

goodcomb = list(NULL)
for( i in 3 : length(varset))
{
  indexcomb = combn(varset,i)
  for( icomb in 1:ncol(indexcomb))
  {
    comb = indexcomb[,icomb]
    print(comb)
    vars = paste0(comb,collapse = "+")
    f = formula(paste('leadclflag ~ ',vars))
    judge = testindex(longtotest,comb,f,testdate,pricedata,analysedata,start,end)
    if(is.null(judge)) next
    goodcomb = append(goodcomb,list(comb))
  }
}


