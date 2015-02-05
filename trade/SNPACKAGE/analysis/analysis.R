#sn跑完后留下的交易明细地址
path = "D:/myreport_0.55/SH000001.TXT/63/per.csv"
txns = read.csv(path)
#只需要时间以及单笔利润
txns = txns[,c(3,8)]
txns = read.zoo(txns,index.column=1)
txns = xts(txns)
names(txns) = 'rtn'

years = list()
index_year = 1
from =as.numeric(strftime(first(index(txns)),'%Y'))
end = as.numeric(strftime(last(index(txns)),'%Y'))
#分析每年的交易数据
for(i in from : end)
{
   year = as.character(i)
   txn = txns[year]
   performance = performanceInfo(txn)
   
   years[[index_year]] = performance
   index_year = index_year + 1
}

#每年盈利数据
netprofitPerYear = sapply(years,function(x){return(x$netProfit)})
#绘图看利润分布
plot(netprofitPerYear)
#盈利年份总数
numProfitYears =length(netprofitPerYear[netprofitPerYear > 0])

mean(netprofitPerYear)
sd(netprofitPerYear)

#每年交易笔数
numTrades = sapply(years,function(x){return(x$totalTrades)})
#交易笔数分布
plot(numTrades)
mean(netprofitPerYear)
sd(netprofitPerYear)

#每年最大回撤
maxDown =  sapply(years,function(x){return(x$mdd)})
mean(maxDown)
sd(maxDown)

#每年连续亏损笔数
consecLossnum =  sapply(years,function(x){return(x$maxLossNum)})

#每年连续亏损金额
consecLossAmount =  sapply(years,function(x){return(x$totalLossDuringMaxLossPeriod)})

#每年胜率
profitRatio = sapply(years,function(x){return(x$percenPositive)})

