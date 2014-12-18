require(quantmod)
require(testthat)

#获取IBM数据
IBM <- getSymbols("IBM",auto.assign=FALSE,from='2010-01-01')
#除权除息
IBM=adjustOHLC(IBM)

#周三周四涨的情况下，周五开盘买入 周五收盘卖出

result = backTestBySnRule(c(0,0,1,1,0),IBM,5,5)

