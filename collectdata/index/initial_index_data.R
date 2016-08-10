require(quantmod)
initial_data_path = 'D:/data/collectdata/index/day/'
#初始化函数,历史数据由csv构成
initial_index_data = function(path,symbol)
{
  data_yahoo = data.frame()
  data = data.frame()
  begindate = '1900-01-01'
  if(path != '')
  {
    data = read.csv(path,head=T)
    data = xts(data[,2:6],order.by = as.Date(data[,1]))
    lastedate = tail(index(data),1)
    begindate = lastedate + 1
  }
  data_yahoo = getSymbols(symbol,auto.assign=F,from=begindate)
  if(nrow(data_yahoo) > 0)
  {
    data_yahoo =adjustOHLC(getSymbols(symbol,auto.assign=F,from=begindate),use.Adjusted = T)
    colnames(data_yahoo) = c('Open','High','Low','Close','Volume','adjust')
    data_yahoo$Close = data_yahoo$adjust
    data_yahoo = data_yahoo[,1:5]
    data = rbind(data,data_yahoo)  
  }
  
  dates = as.character(index(data))
  data = as.data.frame(data)
  data$date = dates
  return(data)
}


#初始化道琼斯指数
path = paste(initial_data_path,'dow.csv',sep='')
data = read.csv(path,head=T)
colnames(data) = c('date','Open','High','Low','Close','Volume')
symbol = '^DJI'
data$date = as.Date(as.character(data$date),format = '%Y%m%d')
data = xts(data[,2:6],order.by = data[,1])
lastedate = tail(index(data),1)
begindate = lastedate + 1
data_yahoo =adjustOHLC(getSymbols(symbol,auto.assign=F,from=begindate),use.Adjusted = T) 
colnames(data_yahoo) = c('Open','High','Low','Close','Volume','adjust')
data_yahoo$Close = data_yahoo$adjust
data_yahoo = data_yahoo[,1:5]

DJI = rbind(data,data_yahoo)
dates = as.character(index(DJI))
DJI = as.data.frame(DJI)
DJI$date = dates
writeToDataBase('DJI',DJI)
#初始化DAX指数 德国法兰克福DAX指数
#1987/12/30基数1000点

path = paste(initial_data_path,'dax_d.csv',sep='')
symbol = '^GDAXI'
data = initial_index_data(path,symbol)
writeToDataBase('DAX',data)

#标准普尔 GSPC指数
path = paste(initial_data_path,'spx_d.csv',sep='')
symbol = '^GSPC'
data = initial_index_data(path,symbol)
writeToDataBase('GSPC',data)


#IXIC 纳斯达克综合指数
path = paste(initial_data_path,'ndq_d.csv',sep='')
symbol = '^IXIC'
data = initial_index_data(path,symbol)
writeToDataBase('IXIC',data)


#日经225指数
#IXIC 纳斯达克综合指数
path = paste(initial_data_path,'nkx_d.csv',sep='')
symbol = '^N225'
data = initial_index_data(path,symbol)
writeToDataBase('N225',data)


#香港恒生 HSI
path = paste(initial_data_path,'hsi_d.csv',sep='')
symbol = '^HSI'
data = initial_index_data(path,symbol)
writeToDataBase('HSI',data)

#台湾台北加权指数
path = paste(initial_data_path,'twse_d.csv',sep='')
symbol = '^TWII'
data = initial_index_data(path,symbol)
writeToDataBase('TWII',data)

#法国CAC40指数
path = paste(initial_data_path,'cac_d.csv',sep='')
symbol = '^FCHI'
data = initial_index_data(path,symbol)
writeToDataBase('FCHI',data)

#英国富时100指数 FTSE
#path = paste(initial_data_path,'x_f_d.csv',sep='')
#symbol = '^FTSE'
#data = initial_index_data(path,symbol)

#000001.SS 上证 
path = paste(initial_data_path,'shc_d.csv',sep='')
symbol = '000001.SS'
data = initial_index_data(path,symbol)
writeToDataBase('shindex',data)

#399001.SZ 深圳
#symbol = '399001.SZ'
#data = initial_index_data('',symbol)
