# -*- coding: utf-8 -*-
import numpy as np
import matplotlib
from matplotlib import pylab, mlab, pyplot
plt = pyplot
from IPython.display import display
from IPython.core.pylabtools import figsize, getfigs
from pylab import *
from numpy import *
import pandas as pd

## 以上库是在执行ipython --pylab 启动时导入的包
import json
##
plot(arange(10))
#data source
#https://github.com/wesm/pydata-book
dirpath = 'd:/pydata-book/'
path =dirpath + 'ch02/usagov_bitly_data2012-03-16-1331923249.txt'
open(path).readline()
records = [json.loads(line) for line in open(path)]

time_zones = [rec['tz'] for rec in records if 'tz' in rec]

def get_counts(sequence):
    counts= {}
    for x in sequence:
        if x in counts:
            counts[x] += 1
        else :
            counts[x] = 1
    return counts 
#库版本
from collections import defaultdict
def get_counts1(sequence):
    counts = defaultdict(int)
    for x in sequence:
        counts[x] += 1
    return counts

counts = get_counts(time_zones)
#按统计个数排序
def top_counts(count_dict,n=10):
    value_key_pairs = [(count,tz) for tz,count in count_dict.items()]
    value_key_pairs.sort()
    return value_key_pairs[-n:]
top_counts(counts)
from collections import Counter
counts = Counter(time_zones)
counts.most_common(10)

from pandas import DataFrame,Series
frame = DataFrame(records)
tz_counts = frame['tz'].value_counts()
#填补空值
clean_tz = frame['tz'].fillna('Missing')
clean_tz[clean_tz == ''] = 'Unknown'
tz_counts = clean_tz.value_counts()
tz_counts[:10].plot(kind='barh',rot=0)
results = Series([x.split()[0] for x in frame.a.dropna()])