# -*- coding: utf-8 -*-
import tushare as ts
# 宏观经济数据
#存款利率
saving_rate = ts.get_deposit_rate()
#贷款利率
loan_rate = ts.get_loan_rate()
#存款准备金率
reserve_rate = ts.get_rrr()
#货币供应量
m_supply = ts.get_money_supply()

#货币供应量(年底余额)

m_supply_y = ts.get_money_supply_bal()

#GDP
gdp_y = ts.get_gdp_year()
gdp_q = ts.get_gdp_quarter()

#三大需求对GDP贡献
gdp_for = ts.get_gdp_for()

#三大产业对GDP拉动
gdp_pull = ts.get_gdp_pull()

#三大产业贡献率
gdp_contrib = ts.get_gdp_contrib()


cpi = ts.get_cpi()

ppi = ts.get_ppi()

df = ts.shibor_data() #取当前年份的数据
#df = ts.shibor_data(2014) #取2014年的数据
df.sort('date', ascending=False).head(10)

df = ts.shibor_quote_data() #取当前年份的数据
#df = ts.shibor_quote_data(2014) #取2014年的数据
df.sort('date', ascending=False).head(10)

#shibo均值
df = ts.shibor_ma_data() #取当前年份的数据
#df = ts.shibor_ma_data(2014) #取2014年的数据
df.sort('date', ascending=False).head(10)

#贷款基础利率
lpr = ts.lpr_data() #取当前年份的数据
#df = ts.lpr_data(2014) #取2014年的数据
