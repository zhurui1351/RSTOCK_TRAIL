# -*- coding: utf-8 -*-
"""
Created on Wed Oct 12 14:35:03 2016

@author: zhu
"""
#encoding=utf-8
import MySQLdb
import pymysql
import pandas as pd
from pandas import DataFrame,Series
import pandas.io.sql as sql
conn = pymysql.connect(host='127.0.0.1',
                             port=3306,
                             user='root',
                             password='123456',
                             db='dap',charset='utf8')
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

#all_tbs = "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = 'dap' AND table_name LIKE 'h%'"
#cursor = conn.cursor()
#cursor.execute(all_tbs)
#results = cursor.fetchall()
#sql_all = ''
#for r in results:
#    tbname = r[0]
#    s = 'select * from ' + tbname
#    sql_all = sql_all + ' union all ' + s 
#
#alldata = 'select * from alldata where pointid = "278657"'
#cursor = conn.cursor()
#cursor.execute(alldata)

pointdata = "SELECT a.*,b.*  FROM alldata a LEFT JOIN ptai b ON  a.PointID = b.PointID  WHERE a.pointid in ('85475469','11813009') and comment = '总有功功率'"
pointdata = "SELECT a.*,b.*  FROM alldata a LEFT JOIN ptai b ON  a.PointID = b.PointID  WHERE  comment = '总有功功率'"

pdata =  sql.read_sql(pointdata,conn)
conn.close()

subpdata = pdata[['PointID','BayName','ADate','ATime','fValue']]
subpdata = subpdata.iloc[:,1:6]
tp = pd.timedelta_range(start='0 days', end='1 days', freq='5T')
tp = tp[0:(len(tp)-1)]

ptable = subpdata.pivot_table(index=['PointID','BayName','ADate'],columns=['ATime'],values='fValue')
ptable_tp = ptable.reindex(columns=tp)
ptable_flat = ptable_tp.reset_index()

kw_names = ['kw'+str(i) for i in range(1,289)]
names = ptable_flat.columns[0:3]
names = names.tolist() + kw_names

ptable_flat.columns = names
#http://python.jobbole.com/81212/

ptable_flat[ptable_flat.PointID=='11813009' & ptable_flat.ADate=='2016-09-27']

