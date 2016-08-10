require(quantmod)

flushdata = function()
{
  path = 'D:/data/collectdata/index'
  
  config = list(c(symbol='000001.SS',file='000001.txt',exp='上证')
             #   ,c(symbol='399001.SZ',file='399001.txt',exp='深圳')
                ,c(symbol='^IXIC',file='DJI.txt',exp='纳斯达克')
                ,c(symbol='^DJI',file='DJI.txt',exp='道琼斯')
                ,c(symbol='^GSPC',file='GSPC.txt',exp = '标准普尔')
                ,c(symbol='^N225',file='N225.txt',exp = '日经225')
                ,c(symbol='^TWII',file='TWII.txt',exp = '台湾台北加权指数')
                ,c(symbol='^HSI',file='HSI.txt',exp = '恒生指数')
                ,c(symbol='^FCHI',file='FCHI.txt',exp = '法国CAC40指数')
                ,c(symbol='^FTSE',file='FTSE.txt',exp = '英国富时100指数')
                ,c(symbol='^GDAXI',file='GDAXI.txt',exp = '德国法兰克福DAX指数')
  )
  
  for(i in 1:length(config))
  {
    cf = config[[i]]
    data =adjustOHLC(getSymbols(cf['symbol'],auto.assign=F,from='1900-01-1'),use.Adjusted = T) 
    f = paste(path,cf['file'],sep='/')
    write.zoo(data,file=f,sep=',')
  }
  
}

