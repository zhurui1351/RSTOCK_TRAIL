simulateTrade = function(date,code,price,fee,amount,type)
{
  #trading log
  logpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/tradelog.csv'
  content = data.frame(date,code,price,fee,amount,type)
  write.table(content,logpath,quote=F,append=T,row.names = F,col.names = F)
  
  #update holding postion
  poslogpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/currentpositionlog.csv'
  currentpos = read.table(poslogpath,head=T,sep=',')
  if(nrow(currentpos) == 0 )
  {
    costprice=(price*amount - fee)/amount
    costprice = ifelse(type=='s',-costprice,costprice)
    currentrow = list(code=code,costprice=costprice,amount=amount)
    currentpos = rbind(currentpos,currentrow, make.row.names = F)
  }
  
  
}



