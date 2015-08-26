simulateTrade = function(id,date,code,price,fee,amount,stopprice,type)
{
  require('dplyr')
  #trading log
  logpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/tradelog.csv'
  content = data.frame(id,date,code,price,fee,amount,stopprice,type)
  
  #update holding postion
  poslogpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/currentpositionlog.csv'
  currentpos = read.table(poslogpath,head=T,sep=',',stringsAsFactors=F)
  if(type == 's' || type == 'b')
  {
    costprice=(price*amount + fee)/amount
    currentrow = list(id=id,code=code,costprice=costprice,stopprice=stopprice,amount=amount,type=type)
    currentpos = rbind(currentpos,currentrow, make.row.names = F)
    
  }# clean buy pos
  else if(type == 'cb')
  {
    tradeid = id
    stockcode= code
    currentposforcode = filter(currentpos,id==tradeid & code==stockcode)
    if(nrow(currentposforcode) != 1 || currentposforcode$type != 'b')
    {
      stop('cant clean buy ,beacause no long position or you have mutilpal rows')
    }
    if(amount > currentposforcode$amount)
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
      newcostprice = (newamount * currentpos$costprice - amount*(price - currentpos$costprice) + fee) / newamount
      currentrow = list(code=code,costprice=newcostprice,amount=newamount,type='b')
      currentpos[currentpos$code ==code && currentpos$type=='b',] = currentrow 
    }
  }# clean sell pos
  else if(type == 'cs')
  {
    tradeid = id
    stockcode= code
    currentposforcode = filter(currentpos,id==tradeid & code==stockcode)
    if(nrow(currentposforcode) != 1 || currentposforcode$type != 's' )
    {
      stop('cant clean sell ,beacause no long position or you have mutilpal rows')
    }
    if(amount > currentposforcode$amount)
    {
      stop('cant clean sell  more than you hold')     
    }
    newamount = currentposforcode$amount - amount
    if(newamount == 0 )
    {
      i = which(currentpos$code ==code && currentpos$type==type)
      currentpos = currentpos[-i,]
    }
    else
    {
      newcostprice = (newamount * currentpos$costprice - amount*(currentpos$costprice-price ) + fee) / newamount
      currentrow = list(code=code,costprice=newcostprice,amount=newamount,type='s')
      currentpos[currentpos$code ==code && currentpos$type=='s',] = currentrow 
    }
  }
  write.table(content,logpath,quote=F,append=T,row.names = F,col.names = F,sep=',')  
  write.table(currentpos,poslogpath,quote=F,row.names = F,col.names = T,sep=',')  
}

updateTrailStop = function(id,date,stopprice)
{
  logpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/currentpositionlog.csv'
  alltrades = read.table(logpath,head=T,sep=',')
  trade = filter(alltrades,id==id)
  if(nrow(trade) == 0) return()
  
  traillogpath = 'D:/Rcode/code/RSTOCK_TRAIL/trade/wensitan/log/trailstoplog.csv'
  PreStopPrice = alltrades[alltrades$id==id,'stopprice']
  content = data.frame(id=id,date=date,stopprice=stopprice,prestopprice=PreStopPrice)
  alltrades[alltrades$id==id,'stopprice'] = stopprice
  
  write.table(alltrades,logpath,quote=F,row.names = F,col.names = T,sep=',')  
  write.table(content,traillogpath,quote=F,append=T,row.names = F,col.names = F,sep=',')  
}


