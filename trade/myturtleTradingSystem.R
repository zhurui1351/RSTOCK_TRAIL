require(quantmod)
require(TTR)
library(blotter)
history<-list()
#path="/home/zhu/Desktop/stock/data/stock"
path="D:/stock/dest"
Sys.setenv(TZ="UTC")
symbols="stockdata"
files<-dir(path)
#files=c('SH603606.TXT')
#f = files[100]
for(f in files)
{
  fname<-file.path(path,f)
  try(stockdata<-read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",fileEncoding="ISO-8859-1",index.column=1),TRUE) 
  colnames(stockdata)<-c("Open","High","Low","Close","Volume","Amount")
  time(stockdata)=as.POSIXct(time(stockdata))
  stockdata=as.xts(stockdata)
  
  # 该表保存与策略有关的交易事务相关信息
  # 将其存放到portfolio对象
  stockdata=stockdata['2014/']
  # 构建指标
  x=get(symbols)
  i=length(index(x))
  if(i<40)
    next
  # 入市 (& System 2 exits)
  x$Min20 <- runMin(x[,grep('Low',colnames(x))], 7)
  x$Max20 <- runMax(x[,grep('High',colnames(x))],7)
  # 离场
  x$Min10 <- runMin(x[,grep('Low',colnames(x))], 3)
  x$Max10 <- runMax(x[,grep('High',colnames(x))],3)  
  # 仓位规模参数c('High','Low','Close')
  x$N <- ATR(x[,c(2,3,4)], n=7, maType=EMA, wilder=TRUE)[,'atr']
  assign(symbols,x)
  # 创建股票列表
  
  CurrentDate=time(x)[i]         
  # 入市（假设以收盘价填入，因此考虑了滑价）
        # 多头仓???
        if( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max20']) ) {
          print(f)
        } 
      
        # 离场和止???
        if( (  ( as.numeric(Lo(x[i-1,]))  <  as.numeric(x[i-2,'Min10'])  ) )
        ) {
         # print(f)
        } 
          # 加到多头仓位
          if( Hi(x[i-1,]) > ( TxnPrice + N * 0.5 ) ) {
          #  print(f)
          }  
}



