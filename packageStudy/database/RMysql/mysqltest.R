library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "everydaystudy", username="zhurui", password="123456",host="127.0.0.1",port=3306)

dbListTables(conn)
dbListFields(conn, "tt")

#写表
dbWriteTable(conn, "mtcars", mtcars[1:10, ])
#插入记录
dbWriteTable(conn, "mtcars", mtcars[11, ],append=T)
#覆盖原表
dbWriteTable(conn, "mtcars", mtcars[12:32, ], row.names=FALSE,overwrite=TRUE)

#读表
dbReadTable(conn, "mtcars")
#查询数据
dbGetQuery(conn, "SELECT * FROM mtcars where mpg>15")
# 执行SQL脚本查询，并分页
res = dbSendQuery(conn, "SELECT * FROM mtcars where mpg>15")
#查看统计信息
summary(res,verbose = TRUE)
#读取数据
d1 <- fetch(res, n = 3)
while (!dbHasCompleted(res)) {
  chunk <- fetch(res, 2)
  print(nrow(chunk))
}
dbClearResult(res)

#删除表
if(dbExistsTable(conn,'t_demo')){
      dbRemoveTable(conn, "t_demo")
  }


#设置字符集,支持中文
dbSendQuery(conn,'SET NAMES gbk')
dbReadTable(conn, "tt")
#写入中文
dbGetQuery(conn,"insert tt values('中文测试');")
dbReadTable(conn, "tt")
xx = data.frame(name=c("小王","小张"))
dbWriteTable(conn, "tt", xx, append=T,row.names=F)
dbReadTable(conn, "tt")
#更新
r = dbGetQuery(conn,"update tt set name = '小刘' where name='d'")
dbDisconnect(conn)

