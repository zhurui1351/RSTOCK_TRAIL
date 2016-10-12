# -*- coding: utf-8 -*-
"""
Created on Wed Oct 12 14:35:03 2016

@author: zhu
"""
import MySQLdb
import pymysql
import pandas as pd
from pandas import DataFrame,Series
import pandas.io.sql as sql
conn = pymysql.connect(host='127.0.0.1',
                             port=3306,
                             user='root',
                             password='123456',
                             db='dap')
head = 'hdata'
ts = pd.date_range('20150930','20161009')
ts = ts.to_series()
ts = ts.apply(str)
f = lambda x: x.replace('-','')[0:8]
ts = ts.apply(f)

results = DataFrame()
sql_all = ''

for t in ts:
    tbname = head+t
    s = 'select * from ' + tbname
   # result = sql.read_sql(s,conn)
   # results = results.append(result)
    sql_all = sql_all + ' union all ' + s 
    print(tbname)

all_tbs = "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = 'dap' AND table_name LIKE 'h%'"
cursor = conn.cursor()
cursor.execute(all_tbs)
results = cursor.fetchall()
sql_all = ''
for r in results:
    tbname = r[0]
    s = 'select * from ' + tbname
    sql_all = sql_all + ' union all ' + s 