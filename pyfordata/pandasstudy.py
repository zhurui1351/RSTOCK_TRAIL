# -*- coding: utf-8 -*-
"""
Created on Thu Oct 13 13:12:47 2016

@author: zhu
"""

import pandas as pd

pricedata =  pd.read_csv('D:/data/collectdata/index/DJI.txt',sep=' ')
pricedata.columns = ['date','open','high','low','close','volume','adjusted']
pricedata.shape
pricedata.dtypes
#选择行数据
pricedata.loc[1]
pricedata.loc[1:4]
pricedata.loc[[2,5,10]]
#返回文件的最后五行
length = pricedata.shape[0]

last_rows = pricedata.loc[length-5:length-1]

#访问列数据
pricedata['open']
pricedata[['open','close']]

#行列出局
pricedata.iloc[0:3,2:4]

