#定位周内数据序列，输入初始化处理后的对象，开始的索引值
#返回从index开始属于同一周的数据的张跌值以及真实数据的个数
getVolatility <- function(priceData,start_index,tradeDays=5)
{
  #初始化返回值，位置代表周几，值： 0表示没有涨跌（如当天没有数据也可理解为涨跌为0）
  #-1代表跌 1代表涨
  #return_data = c(0,0,0,0,0)
  return_data = rep(0,tradeDays)
  
  #真实数据个数 当天有可能休假，在数据中可能不存在当天的数据
  length = 0
  #获取当前数据是第几周
  week = as.integer(priceData[start_index,'fst'])
  #当前数据是周内第几天
  day = as.integer(priceData[start_index,'sec'])
  
  #一周按tradeDays天计算，不足tradeDays天的进行补齐
  weekLength = tradeDays
  if(( nrow(priceData) - start_index + day) < tradeDays)
  {
    weekLength = nrow(priceData) - start_index + day
  }
  
  for(i in day:weekLength)
  {
    currentWeek = as.integer(priceData[start_index+i-day,'fst'])
    #周数已经变化，中断循环
    if(currentWeek != week)
    {
      break
    }
    return_data[i] = priceData[start_index+i-day,'volatility']
    length = length + 1
  }
  #返回值，列表 第一项是当周的涨跌，第二项是实际数据个数
  return(list(return_data,length))
}
