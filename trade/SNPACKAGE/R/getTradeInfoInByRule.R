#根据规则匹配数据，返回交易日的数据，类型（加仓平仓)

getTradeInfoByRule <- function(rule,mydata,buyday,sellday,tradeDays=5)
{
  tradeDayList = list()
  indexTradeDay = 1
  total = nrow(mydata)
  i = 1
  #遍历所有数据
  while(i <= total)
  {
    #获得数据涨跌
    infoInTime = getVolatility(mydata,i,tradeDays)
    #获得所有数据
    dataInOnetime = getData(mydata,i,tradeDays)
    vol = infoInTime[[1]]
    num = infoInTime[[2]]
    i = i + num
    #如果符合规则，获取每周买卖日期，加入返回列表
    if(isSatisfiedRule(data=as.integer(vol),rule=rule))
    {
      timedata = dataInOnetime[[1]]
      #指定交易日没有数据，直接跳过
      if(is.null(timedata[[buyday]]) || is.null(timedata[[sellday]]))
      {
        next
      }
      #加入返回列表，按加仓平仓类型进行区别
      tradeDayList[[indexTradeDay]] = list(data=timedata[[buyday]],type = 'buy')
      indexTradeDay = indexTradeDay + 1
      tradeDayList[[indexTradeDay]] = list(data=timedata[[sellday]],type = 'sell')
      indexTradeDay = indexTradeDay + 1
    }
  }
  return(tradeDayList)
}