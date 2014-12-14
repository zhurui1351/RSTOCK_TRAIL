#根据规则匹配数据，返回交易日的数据，类型（加仓平仓)

getTradeInfoInByRule <- function(rule,mydata,buyday,sellday,tradeDays=5)
{
  tradeDayList = list()
  indexTradeDay = 1
  total = nrow(mydata)
  i = 1
  #遍历所有数据
  while(i <= total)
  {
    #获得该周的数据涨跌
    infoInOneWeek = getVolatilityInOneWeek(mydata,i,tradeDays)
    #获得该周所有数据
    dataInOneWeek = getDataInOneWeek(mydata,i,tradeDays)
    vol = infoInOneWeek[[1]]
    num = infoInOneWeek[[2]]
    i = i + num
    #如果符合规则，获取每周买卖日期，加入返回列表
    if(isSatisfiedRule(data=as.integer(vol),rule=rule))
    {
      weekdata = dataInOneWeek[[1]]
      #指定交易日没有数据，直接跳过
      if(is.null(weekdata[[buyday]]) || is.null(weekdata[[sellday]]))
      {
        next
      }
      #加入返回列表，按加仓平仓类型进行区别
      tradeDayList[[indexTradeDay]] = list(data=weekdata[[buyday]],type = 'buy')
      indexTradeDay = indexTradeDay + 1
      tradeDayList[[indexTradeDay]] = list(data=weekdata[[sellday]],type = 'sell')
      indexTradeDay = indexTradeDay + 1
    }
  }
  return(tradeDayList)
}