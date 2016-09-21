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
cframe = frame[frame.a.notnull()]
operating_systerm = np.where(cframe.a.str.contains('Windows'),'Windows','not Windows')
by_tz_oz = cframe.groupby(['tz',operating_systerm])
agg_counts = by_tz_oz.size().unstack().fillna(0)
indexer = agg_counts.sum(1).argsort()
count_subset = agg_counts.take(indexer)[-10:]

#movie lens数据

unames = ['user_id','gender','age','occupation','zip']
path = dirpath + 'ch02/movielens/users.dat'
users = pd.read_table(path,sep='::',header=None,names=unames)
rnames = ['user_id','movie_id','rating','timestamp']
path = dirpath + 'ch02/movielens/ratings.dat'
ratings = pd.read_table(path,sep='::',header=None,names=rnames)
mnames = ['movie_id','title','genres']
path = dirpath + 'ch02/movielens/movies.dat'
movies = pd.read_table(path,sep='::',header=None,names=mnames)
data = pd.merge(pd.merge(ratings,users),movies)
mean_ratings = data.pivot_table('rating',index='title',columns='gender',aggfunc='mean')
ratings_by_title = data.groupby('title').size()
ratings_by_title[:10]
active_titles = ratings_by_title.index[ratings_by_title>250]
mean_ratings = mean_ratings.ix[active_titles]
#女性最喜欢电话
top_female_ratings = mean_ratings.sort_index(by='F',ascending=False)
mean_ratings['diff'] = mean_ratings['M'] - mean_ratings['F']
sorted_by_diff = mean_ratings.sort_index(by = 'diff')
sorted_by_diff[::-1][:15]
rating_std_by_title = data.groupby('title')['rating'].std()
rating_std_by_title = rating_std_by_title.ix[active_titles]
rating_std_by_title.order(ascending=False)[:10]
#全美婴儿姓名
names1880 = pd.read_csv(dirpath + 'ch02/names/yob1880.txt', names=['name', 'sex', 'births'])
names1880.groupby('sex').births.sum()
years = range(1880, 2011)
pieces = []
columns = ['name', 'sex', 'births']
for year in years:
    path = dirpath + 'ch02/names/yob%d.txt' % year
    frame = pd.read_csv(path, names=columns)

    frame['year'] = year
    pieces.append(frame)
names = pd.concat(pieces, ignore_index=True)
total_births = names.pivot_table('births', index='year',
                                 columns='sex', aggfunc=sum)
total_births.tail()
total_births.plot(title='Total births by sex and year')

def add_prop(group):
    # Integer division floors
    births = group.births.astype(float)

    group['prop'] = births / births.sum()
    return group
names = names.groupby(['year', 'sex']).apply(add_prop)
np.allclose(names.groupby(['year', 'sex']).prop.sum(), 1)

def get_top1000(group):
    return group.sort_index(by='births', ascending=False)[:1000]
grouped = names.groupby(['year', 'sex'])
top1000 = grouped.apply(get_top1000)

pieces = []
for year, group in names.groupby(['year', 'sex']):
    pieces.append(group.sort_index(by='births', ascending=False)[:1000])
top1000 = pd.concat(pieces, ignore_index=True)

top1000.index = np.arange(len(top1000))


#命名趋势
