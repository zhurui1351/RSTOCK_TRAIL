#函数初始化xts对象，加入三列，第一列为每天的涨跌，以-1,0-1表示，-1表示跌，0表示不变，1表示涨
#第二列为每年的第几周，第三列为周几
initialData <- function(data)
{
  if(!is.xts(data))
  {
    stop("data should be a xts object")
  }
  mydata = data
  mydata$volatility = sign(Cl(mydata)-Op(mydata))
  #获取礼拜几信息
  wdays = xts(strftime(index(mydata), "%u"),index(mydata))
  names(wdays) = 'weekday'
  #获取第几周的信息
  weekth = xts( strftime(index(mydata), "%U"),index(mydata))
  names(weekth) = 'weekth'
  #使用merge.xts报错，未知原因
  #mydata <- data.frame(mydata[,c(1,2,3,4,7)],wdays[,1],weekth[,1])
  mydata = data.frame(Op(mydata),Hi(mydata),Lo(mydata),Cl(mydata),mydata[,'volatility'],wdays[,1],weekth[,1])
  mydata = as.xts(mydata)
  #初始化数据处理结束
  return(mydata)
}
