require(quantmod)
initial_data_path = 'D:/data/collectdata/index/day/'

#初始化道琼斯指数
path = paste(initial_data_path,'dow.csv',sep='')
data = read.csv(path,head=T)
colnames(data) = c('code','date','Open','High','Low','Close','Volume')
data$date = as.Date(as.character(data$date),format = '%Y%m%d')
symbol = '^DJI'
data = data[,2:7]
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

#初始化DAX指数 德国法兰克福DAX指数
#1987/12/30基数1000点

path = paste(initial_data_path,'dax_d.csv',sep='')
data = read.csv(path,head=T)
data = xts(data[,2:6],order.by = as.Date(data[,1]))
lastedate = tail(index(data),1)
begindate = lastedate + 1
symbol = '^GDAXI'
data_yahoo =adjustOHLC(getSymbols(symbol,auto.assign=F,from=begindate),use.Adjusted = T) 
colnames(data_yahoo) = c('Open','High','Low','Close','Volume','adjust')
data_yahoo$Close = data_yahoo$adjust
data_yahoo = data_yahoo[,1:5]
DAX = rbind(data,data_yahoo)
dates = as.character(index(DAX))
DAX = as.data.frame(DAX)
DAX$date = dates

#标准普尔 GSPC指数
path = paste(initial_data_path,'spx_d.csv',sep='')
data = read.csv(path,head=T)
data = xts(data[,2:6],order.by = as.Date(data[,1]))
lastedate = tail(index(data),1)
begindate = lastedate + 1
symbol = '^GSPC'
data_yahoo =adjustOHLC(getSymbols(symbol,auto.assign=F,from=begindate),use.Adjusted = T) 
colnames(data_yahoo) = c('Open','High','Low','Close','Volume','adjust')
data_yahoo$Close = data_yahoo$adjust
data_yahoo = data_yahoo[,1:5]
GSPC = rbind(data,data_yahoo)
dates = as.character(index(GSPC))
GSPC = as.data.frame(GSPC)
GSPC$date = dates

#IXIC 纳斯达克综合指数
path = paste(initial_data_path,'ndq_d.csv',sep='')
data = read.csv(path,head=T)
data = xts(data[,2:6],order.by = as.Date(data[,1]))
IXIC = data
lastedate = tail(index(data),1)
begindate = lastedate + 1
symbol = '^IXIC'
data_yahoo = getSymbols(symbol,auto.assign=F,from=begindate)
if(nrow(data_yahoo) > 0)
{
  data_yahoo =adjustOHLC(getSymbols(symbol,auto.assign=F,from=begindate),use.Adjusted = T)
  colnames(data_yahoo) = c('Open','High','Low','Close','Volume','adjust')
  data_yahoo$Close = data_yahoo$adjust
  data_yahoo = data_yahoo[,1:5]
  IXIC = rbind(data,data_yahoo)  
}

dates = as.character(index(IXIC))
IXIC = as.data.frame(IXIC)
IXIC$date = dates

#日经225指数
#IXIC 纳斯达克综合指数
path = paste(initial_data_path,'nkx_d.csv',sep='')
data = read.csv(path,head=T)
data = xts(data[,2:6],order.by = as.Date(data[,1]))
N225 = data
lastedate = tail(index(data),1)
begindate = lastedate + 1
symbol = '^N225'
data_yahoo = getSymbols(symbol,auto.assign=F,from=begindate)
if(nrow(data_yahoo) > 0)
{
  data_yahoo =adjustOHLC(getSymbols(symbol,auto.assign=F,from=begindate),use.Adjusted = T)
  colnames(data_yahoo) = c('Open','High','Low','Close','Volume','adjust')
  data_yahoo$Close = data_yahoo$adjust
  data_yahoo = data_yahoo[,1:5]
  N225 = rbind(data,data_yahoo)  
}

dates = as.character(index(N225))
N225 = as.data.frame(N225)
N225$date = dates

#香港恒生 HSI
path = paste(initial_data_path,'hsi_d.csv',sep='')
data = read.csv(path,head=T)
data = xts(data[,2:6],order.by = as.Date(data[,1]))
HSI = data
lastedate = tail(index(data),1)
begindate = lastedate + 1
symbol = '^N225'
data_yahoo = getSymbols(symbol,auto.assign=F,from=begindate)
if(nrow(data_yahoo) > 0)
{
  data_yahoo =adjustOHLC(getSymbols(symbol,auto.assign=F,from=begindate),use.Adjusted = T)
  colnames(data_yahoo) = c('Open','High','Low','Close','Volume','adjust')
  data_yahoo$Close = data_yahoo$adjust
  data_yahoo = data_yahoo[,1:5]
  HSI = rbind(data,data_yahoo)  
}

dates = as.character(index(N225))
HSI = as.data.frame(N225)
HSI$date = dates


dbname= "global_index"
username="root"
password = '123456'
host = '127.0.0.1'
port = 3306
conn <- dbConnect(MySQL(), dbname = dbname, username=username, password=password,host=host,port=port)
dbWriteTable(conn, "DJI", DJI,overwrite = T,row.names=F,field.types = list(Open='decimal(10,2)',High='decimal(10,2)',Low='decimal(10,2)',Close='decimal(10,2)',Volume='decimal(10,2)',date='Date'))
dbDisconnect(conn)
