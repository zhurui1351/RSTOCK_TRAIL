# -*- coding: utf-8 -*-
"""
Created on Tue Dec 27 11:23:02 2016

@author: zhu
"""
import pandas as pd
import numpy as np

from datetime import datetime
from pandas.tseries.offsets import *
from pandas import DataFrame,Series
from sys import path
#添加代码路径，以便引用其他文件
path.append(r'D:\wanyi_codebase')
from  src.strategy.adjust_peak_strategy.adjust_peak_strategy import *
from src.strategy.trading_strategy.trading_strategy import  *
from src.utilities.utilities import *
from src.com.load_profile import *
from src.com.price import *
from src.analysis.compute.basic_analysis import *
from sklearn import linear_model
from src.analysis.datamining.cluster.load_cluster import *
reg = linear_model.LinearRegression()
reg.fit ([[0, 0], [1, 1], [2, 2]], [0, 1, 2])
reg.coef_

usage_file = 'D:/wanyi_codebase/src/test_data/storage_data_sets.xlsx'
load_test = Load_Profile(usage_file,id_col=0,start_col = 3,date_col = 1,freq='15T',sheets = [0])
load_test.init()

data = load_test.data

reg.fit(data[['23:15:00','23:00:00']],data['23:30:00'])
reg.coef_
