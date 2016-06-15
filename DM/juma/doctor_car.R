require(RMySQL)
dt = read.csv('C:/Users/ganlu/Desktop/test.csv',head=T)
dt[dt=='\\'] = NA



#conn <- dbConnect(MySQL(), dbname = "report", username="report", password="report",host="10.101.0.102",port=3306)
#dbSendQuery(conn,'SET NAMES gbk')
#dbReadTable(conn, "CUSTOMER_INFO") 
#dbDisconnect(conn)
