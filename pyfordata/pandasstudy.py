# -*- coding: utf-8 -*-
"""
Created on Thu Oct 13 13:12:47 2016

@author: zhu
"""

import pandas as pd
from numpy import *
import operator
from pandas import Series
import numpy as np
from pandas import DataFrame
import matplotlib.pyplot as plt

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

#时间序列
from datetime import datetime
now = datetime.now()
now

now.year, now.month, now.day
delta = datetime(2011, 1, 7) - datetime(2008, 6, 24, 8, 15)
delta.days
delta.seconds
from datetime import timedelta
start = datetime(2011, 1, 7)
start + timedelta(12)
start - 2 * timedelta(12)
stamp = datetime(2011, 1, 3)
str(stamp)

stamp.strftime('%Y-%m-%d')
value = '2011-01-03'
datetime.strptime(value, '%Y-%m-%d')
datestrs = ['7/6/2011', '8/6/2011']
[datetime.strptime(x, '%m/%d/%Y') for x in datestrs]

from dateutil.parser import parse
parse('2011-01-03')
parse('Jan 31, 1997 10:45 PM')
pd.to_datetime(datestrs)
idx = pd.to_datetime(datestrs + [None])
idx
pd.isnull(idx)
#时间序列
dates = [datetime(2011, 1, 2), datetime(2011, 1, 5), datetime(2011, 1, 7),
         datetime(2011, 1, 8), datetime(2011, 1, 10), datetime(2011, 1, 12)]
ts = Series(np.random.randn(6), index=dates)
ts
type(ts)
ts.index
ts + ts[::2]
ts.index.dtype
stamp = ts.index[0]
stamp

ts['2011']
ts['1/10/2011']
ts['20110110']

longer_ts = Series(np.random.randn(1000),
                   index=pd.date_range('1/1/2000', periods=1000))
longer_ts
longer_ts['2001']
longer_ts['2001-05']
ts[datetime(2011, 1, 7):]
ts['1/6/2011':'1/11/2011']
ts.truncate(after='1/9/2011')
dates = pd.date_range('1/1/2000', periods=100, freq='W-WED')
long_df = DataFrame(np.random.randn(100, 4),
                    index=dates,
                    columns=['Colorado', 'Texas', 'New York', 'Ohio'])
long_df.ix['5-2001']
#重复索引
dates = pd.DatetimeIndex(['1/1/2000', '1/2/2000', '1/2/2000', '1/2/2000',
                          '1/3/2000'])
dup_ts = Series(np.arange(5), index=dates)
dup_ts
dup_ts.index.is_unique
dup_ts['1/3/2000']
grouped = dup_ts.groupby(level=0)
grouped.mean()
grouped.count()
#频率转换
ts
ts.resample('D')
index = pd.date_range('4/1/2012', '6/1/2012')
pd.date_range(start='4/1/2012', periods=20)
pd.date_range('1/1/2000', '12/1/2000', freq='BM')
pd.date_range('5/2/2012 12:56:31', periods=5)

pd.date_range('5/2/2012 12:56:31', periods=5, normalize=True)
from pandas.tseries.offsets import Hour, Minute
hour = Hour()
hour
four_hours = Hour(4)
four_hours

pd.date_range('1/1/2000', '1/3/2000 23:59', freq='4h')
Hour(2) + Minute(30)
pd.date_range('1/1/2000', periods=10, freq='1h30min')
rng = pd.date_range('1/1/2012', '9/1/2012', freq='WOM-3FRI')
list(rng)

#移动或超前
ts = Series(np.random.randn(4),
            index=pd.date_range('1/1/2000', periods=4, freq='M'))
ts
ts.shift(2)
ts.shift(-2)
ts / ts.shift(1) - 1
ts.shift(2, freq='M')
ts.shift(3, freq='D')
ts.shift(1, freq='3D')
ts.shift(1, freq='90T')
from pandas.tseries.offsets import Day, MonthEnd
now = datetime(2011, 11, 17)
now + 3 * Day()
now + MonthEnd()
now + MonthEnd(2)
offset = MonthEnd()
offset.rollforward(now)
offset.rollback(now)
ts = Series(np.random.randn(20),
            index=pd.date_range('1/15/2000', periods=20, freq='4d'))
ts.groupby(offset.rollforward).mean()
ts.resample('M', how='mean')
import pytz
pytz.common_timezones[-5:]
tz = pytz.timezone('US/Eastern')
tz
#时期
p = pd.Period(2007, freq='A-DEC')
p
p+5
p-2
pd.Period('2014', freq='A-DEC') - p
rng = pd.period_range('1/1/2000', '6/30/2000', freq='M')
rng
Series(np.random.randn(6), index=rng)
values = ['2001Q3', '2002Q2', '2003Q1']
index = pd.PeriodIndex(values, freq='Q-DEC')
index

p = pd.Period('2007', freq='A-DEC')
p.asfreq('M', how='start')
p.asfreq('M', how='end')

p = pd.Period('2007', freq='A-JUN')
p.asfreq('M', 'start')

p.asfreq('M', 'end')
p = pd.Period('Aug-2007', 'M')
p.asfreq('A-JUN')
rng = pd.period_range('2006', '2009', freq='A-DEC')
ts = Series(np.random.randn(len(rng)), index=rng)
ts
ts.asfreq('M', how='start')
ts.asfreq('B', how='end')

p = pd.Period('2012Q4', freq='Q-JAN')
p
p.asfreq('D', 'start')
p.asfreq('D', 'end')
p4pm = (p.asfreq('B', 'e') - 1).asfreq('T', 's') + 16 * 60
p4pm
p4pm.to_timestamp()
rng = pd.period_range('2011Q3', '2012Q4', freq='Q-JAN')
ts = Series(np.arange(len(rng)), index=rng)
ts
new_rng = (rng.asfreq('B', 'e') - 1).asfreq('T', 's') + 16 * 60
ts.index = new_rng.to_timestamp()
ts
rng = pd.date_range('1/1/2000', periods=3, freq='M')
from numpy.random import randn
ts = Series(randn(3), index=rng)
pts = ts.to_period()
ts
pts
rng = pd.date_range('1/29/2000', periods=6, freq='D')
ts2 = Series(randn(6), index=rng)
ts2.to_period('M')
pts = ts.to_period()
pts
pts.to_timestamp(how='end')

#重抽样
rng = pd.date_range('1/1/2000', periods=100, freq='D')
ts = Series(randn(len(rng)), index=rng)
ts.resample('M', how='mean')
ts.resample('M', how='mean', kind='period')

#分钟数据
rng = pd.date_range('1/1/2000', periods=12, freq='T')
ts = Series(np.arange(12), index=rng)
ts
ts.resample('5min', how='sum')
ts.resample('5min', how='sum', closed='left')
ts.resample('5min', how='sum', closed='left', label='left')
ts.resample('5min', how='sum', loffset='-1s')
ts.resample('5min', how='ohlc')
rng = pd.date_range('1/1/2000', periods=100, freq='D')
ts = Series(np.arange(100), index=rng)
ts.groupby(lambda x: x.month).mean()
ts.groupby(lambda x: x.weekday).mean()
#升采样
frame = DataFrame(np.random.randn(2, 4),
                  index=pd.date_range('1/1/2000', periods=2, freq='W-WED'),
                  columns=['Colorado', 'Texas', 'New York', 'Ohio'])
frame

df_daily = frame.resample('D')
df_daily
frame.resample('D', fill_method='ffill')
frame.resample('D', fill_method='ffill', limit=2)
frame.resample('W-THU', fill_method='ffill')

frame = DataFrame(np.random.randn(24, 4),
                  index=pd.period_range('1-2000', '12-2001', freq='M'),
                  columns=['Colorado', 'Texas', 'New York', 'Ohio'])
frame[:5]

annual_frame = frame.resample('A-DEC', how='mean')
annual_frame
annual_frame.resample('Q-DEC', fill_method='ffill')
annual_frame.resample('Q-DEC', fill_method='ffill', convention='start')
annual_frame.resample('Q-MAR', fill_method='ffill')

# 绘图
dirpath = 'd:/pydata-book/'
close_px_all = pd.read_csv(dirpath+'ch09/stock_px.csv', parse_dates=True, index_col=0)
close_px = close_px_all[['AAPL', 'MSFT', 'XOM']]

close_px = close_px.resample('B', fill_method='ffill')
close_px['AAPL'].plot()
close_px.ix['2009'].plot()
close_px['AAPL'].ix['01-2011':'03-2011'].plot()
appl_q = close_px['AAPL'].resample('Q-DEC', fill_method='ffill')
appl_q.ix['2009':].plot()

#移动时间窗口
close_px = close_px.asfreq('B').fillna(method='ffill')
close_px.AAPL.plot()
pd.rolling_mean(close_px.AAPL, 250).plot()
plt.figure()

appl_std250 = pd.rolling_std(close_px.AAPL, 250, min_periods=10)
appl_std250[5:12]
appl_std250.plot()
expanding_mean = lambda x: rolling_mean(x, len(x), min_periods=1)
pd.rolling_mean(close_px, 60).plot(logy=True)
plt.close('all')
fig, axes = plt.subplots(nrows=2, ncols=1, sharex=True, sharey=True,
                         figsize=(12, 7))

aapl_px = close_px.AAPL['2005':'2009']

ma60 = pd.rolling_mean(aapl_px, 60, min_periods=50)
ewma60 = pd.ewma(aapl_px, span=60)

aapl_px.plot(style='k-', ax=axes[0])
ma60.plot(style='k--', ax=axes[0])
aapl_px.plot(style='k-', ax=axes[1])
ewma60.plot(style='k--', ax=axes[1])
axes[0].set_title('Simple MA')
axes[1].set_title('Exponentially-weighted MA')
#二元移动平均
close_px
spx_px = close_px_all['SPX']
spx_rets = spx_px / spx_px.shift(1) - 1
returns = close_px.pct_change()
corr = pd.rolling_corr(returns.AAPL, spx_rets, 125, min_periods=100)
corr.plot()
corr = pd.rolling_corr(returns, spx_rets, 125, min_periods=100)
corr.plot()
#自定义移动平均

from scipy.stats import percentileofscore
score_at_2percent = lambda x: percentileofscore(x, 0.02)
result = pd.rolling_apply(returns.AAPL, 250, score_at_2percent)
result.plot()



#聚合分组

df = DataFrame({'key1' : ['a', 'a', 'b', 'b', 'a'],
                'key2' : ['one', 'two', 'one', 'two', 'one'],
                'data1' : np.random.randn(5),
                'data2' : np.random.randn(5)})
df

grouped = df['data1'].groupby(df['key1'])
grouped
grouped.mean()
means = df['data1'].groupby([df['key1'], df['key2']]).mean()
means
means.unstack()

states = np.array(['Ohio', 'California', 'California', 'Ohio', 'Ohio'])
years = np.array([2005, 2005, 2006, 2005, 2006])
df['data1'].groupby([states, years]).mean()
df.groupby('key1').mean()
df.groupby(['key1', 'key2']).mean()
df.groupby(['key1', 'key2']).size()

for name, group in df.groupby('key1'):
    print(name)
    print(group)
    
for (k1, k2), group in df.groupby(['key1', 'key2']):
    print((k1, k2))
    print(group)
    
pieces = dict(list(df.groupby('key1')))
pieces['b']
df.dtypes
grouped = df.groupby(df.dtypes, axis=1)
dict(list(grouped))

df.groupby(['key1', 'key2'])[['data2']].mean()
s_grouped = df.groupby(['key1', 'key2'])['data2']
s_grouped
s_grouped.mean()


people = DataFrame(np.random.randn(5, 5),
                   columns=['a', 'b', 'c', 'd', 'e'],
                   index=['Joe', 'Steve', 'Wes', 'Jim', 'Travis'])
people.ix[2:3, ['b', 'c']] = np.nan # Add a few NA values
people

mapping = {'a': 'red', 'b': 'red', 'c': 'blue',
           'd': 'blue', 'e': 'red', 'f' : 'orange'}
           
by_column = people.groupby(mapping, axis=1)
by_column.sum()


map_series = Series(mapping)
map_series

people.groupby(map_series, axis=1).count()
#根据函数进行分组
people.groupby(len).sum()
key_list = ['one', 'one', 'one', 'two', 'two']
people.groupby([len, key_list]).min()

columns = pd.MultiIndex.from_arrays([['US', 'US', 'US', 'JP', 'JP'],
                                    [1, 3, 5, 1, 3]], names=['cty', 'tenor'])
hier_df = DataFrame(np.random.randn(4, 5), columns=columns)
hier_df
hier_df.groupby(level='cty', axis=1).count()
#数据聚合
df
grouped = df.groupby('key1')
grouped['data1'].quantile(0.9)
#自定义
def peak_to_peak(arr):
    return arr.max() - arr.min()
grouped.agg(peak_to_peak)
    grouped.describe()

tips = pd.read_csv(dirpath + 'ch08/tips.csv')
tips['tip_pct'] = tips['tip'] / tips['total_bill']
tips[:6]
grouped = tips.groupby(['sex', 'smoker'])
grouped_pct = grouped['tip_pct']
grouped_pct.agg('mean')
grouped_pct.agg(['mean', 'std', peak_to_peak])
grouped_pct.agg([('foo', 'mean'), ('bar', np.std)])
functions = ['count', 'mean', 'max']
result = grouped['tip_pct', 'total_bill'].agg(functions)
result
result['tip_pct']
ftuples = [('Durchschnitt', 'mean'), ('Abweichung', np.var)]
grouped['tip_pct', 'total_bill'].agg(ftuples)
grouped.agg({'tip' : np.max, 'size' : 'sum'})
grouped.agg({'tip_pct' : ['min', 'max', 'mean', 'std'],
             'size' : 'sum'})
             
tips.groupby(['sex', 'smoker'], as_index=False).mean()

#apply transform
df
k1_means = df.groupby('key1').mean().add_prefix('mean_')
k1_means
pd.merge(df, k1_means, left_on='key1', right_index=True)
key = ['one', 'two', 'one', 'two', 'one']
people.groupby(key).mean()
people.groupby(key).transform(np.mean)
def demean(arr):
    return arr - arr.mean()
demeaned = people.groupby(key).transform(demean)
demeaned
demeaned.groupby(key).mean()
def top(df, n=5, column='tip_pct'):
    return df.sort_index(by=column)[-n:]
top(tips, n=6)
tips.groupby('smoker').apply(top)
tips.groupby(['smoker', 'day']).apply(top, n=1, column='total_bill')

result = tips.groupby('smoker')['tip_pct'].describe()
result
result.unstack('smoker')

#金融时间序列
#时间补齐

