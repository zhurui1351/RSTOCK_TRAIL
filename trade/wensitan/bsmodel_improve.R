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
pricedata = readSHindex()
pricedata = na.omit(pricedata)
#处理待预测数据，用lead提前一天，把头天的涨跌加入预测变量
cl = Cl(pricedata) - Op(pricedata)
clflag = ifelse(cl < 0 ,'down','up')
leadclflag = shift(clflag,n=1,type='lead')
analysedata = merge(leadclflag,clflag)

start = '1995'
end = '2000'
analysedata_train = analysedata['2000/2005']
pricedata_train = pricedata['2000/2005']
