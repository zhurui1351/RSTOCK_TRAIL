#判断周期内每日涨跌是否符合指定规则

isSatisfiedRule<- function(data,rule)
{
  for(i in 1 : length(rule))
  {
    #规则为0，代表不考虑当天的涨跌
    if(rule[i] == 0)
    {
      next
    }
    #不满足规则
    if(rule[i] != data[i])
    {
      return(FALSE)
    }
  }
  return(TRUE)
}