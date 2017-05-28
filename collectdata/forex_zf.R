library(RMySQL)

con <- dbConnect(MySQL(),host="115.159.85.155",dbname="forexnews",user="zfly",password="123456")

con <- dbConnect(MySQL(),host="115.159.85.155",dbname="forexnews",user="zhurui",password="123456")

summary(con)
dbGetInfo(con)
dbListTables(con)

dbSendQuery(con, "SET NAMES gbk")
indexdata <- dbReadTable(conn = con, name = "fnewspannel_ecoindicator")
head(indexdata)

help(package="RMySQL")
con <- dbConnect(MySQL(),host="115.159.85.155",dbname="forexnews",user="zfly",password="123456")

summary(con)
dbGetInfo(con)
dbListTables(con)

dbSendQuery(con, "SET NAMES utf8")

fin.indicator <- dbReadTable(conn = con, name = "fnewspannel_finindicator")
head(fin.indicator)

inter.holiday <- dbReadTable(conn = con, name = "fnewspannel_interholiday")
head(inter.holiday)

big.event <- dbReadTable(conn = con, name = "fnewspannel_bigevent")
head(big.event)

bank.dynamics <- dbReadTable(conn = con, name = "fnewspannel_bankdynamics")
head(bank.dynamics)

dbDisconnect(conn = con)
