rm(list=ls(all=T))
source('D:/Rcode/code/RSTOCK_TRAIL/collectdata/include.R',encoding='utf8')
types = list(Open='decimal(10,2)',High='decimal(10,2)',Low='decimal(10,2)',Close='decimal(10,2)',Vol='decimal(10,2)',Oi='decimal(10,2)',time='time')
dbname= "commodity_cn_continue_m"

doubo_m = read_m_1m_taobao()
times = index(doubo_m)
doubo_m = as.data.frame(doubo_m)
doubo_m$time = times
writeToDataBase('doubo',doubo_m,types=types,dbname=dbname)

douyou_m = read_y_1m_taobao()
times = index(douyou_m)
douyou_m = as.data.frame(douyou_m)
douyou_m$time = times
writeToDataBase('douyou',douyou_m,types=types,dbname=dbname)

corp_m = read_c_1m_taobao()
times = index(corp_m)
corp_m = as.data.frame(corp_m)
corp_m$time = times
writeToDataBase('corp',corp_m,types=types,dbname=dbname)

dou1_m = read_s1_1m_taobao()
times = index(dou1_m)
dou1_m = as.data.frame(dou1_m)
dou1_m$time = times
writeToDataBase('dou1',dou1_m,types=types,dbname=dbname)

