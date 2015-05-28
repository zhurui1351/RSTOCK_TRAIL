library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "test", username="root", password="123456",host="127.0.0.1",port=3306)
dbReadTable(conn, "tt")
dbDisconnect(conn)
