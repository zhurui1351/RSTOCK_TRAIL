require(quantmod)
initial_data_path = 'D:/data/collectdata/index/dow/dow.csv'
data = read.csv(initial_data_path,head=T)
colnames(data) = c('code','date','Open','High','Low','Close','Volume')
data$date = as.Date(as.character(data$date),format = '%Y%m%d')
tail(data)
symbol = '^DJI'
data = data[,2:7]
data = xts(data[,2:6],order.by = data[,1])
data_yahoo =adjustOHLC(getSymbols(symbol,auto.assign=F,from='2001-01-27'),use.Adjusted = T) 
colnames(data_yahoo) = c('Open','High','Low','Close','Volume','adjust')
data_yahoo$Close = data_yahoo$adjust
data_yahoo = data_yahoo[,1:5]

DJI = rbind(data,data_yahoo)
dates = as.character(index(DJI))
DJI = as.data.frame(DJI)
DJI$date = dates


dbname= "global_index"
username="root"
password = '123456'
host = '127.0.0.1'
port = 3306
conn <- dbConnect(MySQL(), dbname = dbname, username=username, password=password,host=host,port=port)
dbWriteTable(conn, "DJI", DJI,overwrite = T,row.names=F,field.types = list(Open='decimal(10,2)',High='decimal(10,2)',Low='decimal(10,2)',Close='decimal(10,2)',Volume='decimal(10,2)',date='Date'))
dbDisconnect(conn)
