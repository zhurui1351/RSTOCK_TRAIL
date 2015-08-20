showcurrentPostionforcode = function(date,stockcode,currentpos)
{
  currentposforcode = filter(currentpos,code==stockcode)
  if(nrow(currentposforcode) == 0)
    return();
  price  = readOneStock(stockcode)
  if(is.null(price))
  {
    return();
  }
  cp = price[date]
  if(nrow(cp) == 0)
  {
    dates = index(price)
    i = findInterval(as.Date(date),as.Date(dates))
    cp = price[dates[i]] 
  }
  cp$holdamount = currentposforcode$amount
  cp$costprice = currentposforcode$costprice
  cp$stopprice = currentposforcode$stopprice
  cp$profit = ( cp$Close - cp$costprice)*cp$holdamount 
  cp = transform(cp,type=currentposforcode$type)
  print(cp)
  return(cp)
}

showcurrentPostion = function(date,code='all')
{
  require('dplyr')
  poslogpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/currentpositionlog.csv'
  currentpos = read.table(poslogpath,head=T,sep=',',stringsAsFactors=F)
  if(nrow(currentpos) == 0) return()
  r = list()
  if(code == 'all')
  {
    codelist = as.character(currentpos$code)
    r=lapply(codelist,FUN=function(x){showcurrentPostionforcode(date,x,currentpos)})
  }
  else
  {
    r=showcurrentPostionforcode(date,code,currentpos)
  }
  return(r)
}

