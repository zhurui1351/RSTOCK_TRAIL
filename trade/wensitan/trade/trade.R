simulateTrade = function(date,code,price,fee,amount,type)
{
  require('dplyr')
  #trading log
  logpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/tradelog.csv'
  content = data.frame(date,code,price,fee,amount,type)
  write.table(content,logpath,quote=F,append=T,row.names = F,col.names = F)
  
  #update holding postion
  poslogpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/currentpositionlog.csv'
  currentpos = read.table(poslogpath,head=T,sep=',')
  if(type == 's' || type == 'b')
  {
    currentposforcode = filter(currentpos,code==code && type ==type)
    if(nrow(currentpos) == 0 )
    {
      costprice=(price*amount + fee)/amount
      currentrow = list(code=code,costprice=costprice,amount=amount,type=type)
      currentpos = rbind(currentpos,currentrow, make.row.names = F)
    }
    else if(nrow(currentpos) == 1)
    {
      newamount = currentposforcode$amount + amount
      newcostprice = (currentposforcode$costprice * currentposforcode$amount +  price*amount + fee) / newamount
      currentrow = list(code=code,costprice=newcostprice,amount=newamount,type=type)
      currentpos[currentpos$code ==code && currentpos$type==type,] = currentrow
    }
    
  }# clean buy pos
  else if(type = 'cb')
  {
    currentposforcode = filter(currentpos,code==code && type == 'b')
    if(nrow(currentpos) != 0 )
    {
      stop('cant clean buy ,beacause no long position or you have mutilpal rows')
    }
    if(amount > currentposforcode)
    {
      stop('cant clean buy  more than you hold')     
    }
    newamount = currentposforcode$amount - amount
    if(newamount == 0 )
    {
      i = which(currentpos$code ==code && currentpos$type==type)
      currentpos = currentpos[-i,]
    }
    else
    {
      newcostprice = ( price*amount - currentposforcode$costprice * currentposforcode$amount + fee) / newamount
      currentrow = list(code=code,costprice=newcostprice,amount=newamount,type=type)
      currentpos[currentpos$code ==code && currentpos$type=='b',] = currentrow 
    }
  }# clean sell pos
  else if(type = 'cb')
  {
    currentposforcode = filter(currentpos,code==code && type == 's')
    if(nrow(currentpos) != 0 )
    {
      stop('cant clean buy ,beacause no long position or you have mutilpal rows')
    }
    if(amount > currentposforcode)
    {
      stop('cant clean buy  more than you hold')     
    }
    newamount = currentposforcode$amount - amount
    if(newamount == 0 )
    {
      i = which(currentpos$code ==code && currentpos$type==type)
      currentpos = currentpos[-i,]
    }
    else
    {
      newcostprice = (  currentposforcode$costprice * currentposforcode$amount -price*amount  + fee) / newamount
      currentrow = list(code=code,costprice=newcostprice,amount=newamount,type=type)
      currentpos[currentpos$code ==code && currentpos$type=='s',] = currentrow 
    }
  }
  
  write.table(currentpos,poslogpath,quote=F,row.names = F,col.names = T,sep=',')
  
}



