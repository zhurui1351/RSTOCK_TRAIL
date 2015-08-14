tradelogAnalysis = function(stockcode)
{
  require(dplyr)
  logpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/tradelog.csv'
  tradelog = read.table(logpath,head=T,sep=',')
  codelog = filter(tradelog,code==stockcode)
  if(nrow(codelog) == 0) return()
  groupid = 1
  currentamount = 0
  id = c()
  for(i in 1:nrow(codelog))
  {
    cur = codelog[i,]
    if(cur$type == 'b')
    {
      currentamount = currentamount + cur$amount
      id = c(id,groupid)
    }
    else if(cur$type == 'cb')
    {
      currentamount = currentamount - cur$amount
      id = c(id,groupid)
      if(currentamount == 0)
      {
        groupid = groupid + 1
      }
    }
  }
  codelog$groupid = id
}

