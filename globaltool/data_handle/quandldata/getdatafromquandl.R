getSoybean_us = function()
{
  Quandl.api_key('8z-Fi9CYwvbvDPXw8v4R')
  names = c('SF','SH','SK','SN','SQ','SU','SX')
  path = 'D:/data/collectdata/quandldata/commidity/soybean/'
  for(name in names)
  {
    for(i in 1970:2016)
    {
      contract = paste(name,i,sep='')
      code = paste('CME',contract,sep='/')
      price = Quandl(code)
      if(nrow(price == 0))
      {
        warning('no data for ',code)
        return;
      }
      print(code)
      write.csv(price,file = paste(path,code,'.txt',sep=''),row.names=F,quote=F )
      Sys.sleep(5)
    }
  }
}