writeToDataBase = function(tablename,data,types = list(Open='decimal(10,2)',High='decimal(10,2)',Low='decimal(10,2)',Close='decimal(10,2)',Volume='decimal(10,2)',date='Date'), dbname= "global_index")
{
  username="root"
  password = '123456'
  host = '127.0.0.1'
  port = 3306
  conn <- dbConnect(MySQL(), dbname = dbname, username=username, password=password,host=host,port=port)
  dbWriteTable(conn,tablename, data,overwrite = T,row.names=F,field.types = types)
  dbDisconnect(conn)
}

readFromDataBase = function(tablename,dbname= "global_index")
{
  username="root"
  password = '123456'
  host = '127.0.0.1'
  port = 3306
  conn <- dbConnect(MySQL(), dbname = dbname, username=username, password=password,host=host,port=port)
  data = dbReadTable(conn,tablename)
  dbDisconnect(conn)
  return(data)
}

insertToDataBase = function(tablename,data,types = list(Open='decimal(10,2)',High='decimal(10,2)',Low='decimal(10,2)',Close='decimal(10,2)',Volume='decimal(10,2)',date='Date'), dbname= "global_index")
{
  username="root"
  password = '123456'
  host = '127.0.0.1'
  port = 3306
  conn <- dbConnect(MySQL(), dbname = dbname, username=username, password=password,host=host,port=port)
  dbWriteTable(conn,tablename, data,append = T,row.names=F,field.types = types)
  dbDisconnect(conn)
}
