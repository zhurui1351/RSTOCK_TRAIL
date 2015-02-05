require(quantmod)
require(testthat)

trades=ts(c(1,2,3,4,5,-1,-2,-3,-4,-5,-1,1,-5), frequency = 12, start = c(2012, 2))
trades = as.xts(trades)
index(trades) <- as.POSIXct(index(trades))
drawDonwInfo = performanceInfo(trades)

#与手工计算的做对比
#每笔的回撤
expect_equal(drawDonwInfo$dd,c(0,0,0,0,0,1,3,6,10,15,16,15,20))
#最大回撤金额
expect_equal(drawDonwInfo$mdd,20)
#最大回撤开始时间
expect_equal(as.character(drawDonwInfo$maxDrawDownFrom),'2012-06-01')
#最大回撤结束时间
expect_equal(as.character(drawDonwInfo$maxDrawDownTo),'2013-02-01')
#最大连续亏损笔数
expect_equal(drawDonwInfo$maxLossNum,6)
#最大连续亏损盈利
expect_equal(drawDonwInfo$totalLossDuringMaxLossPeriod,-16)
#最大连续亏损开始时间
expect_equal(as.character(drawDonwInfo$consecLossFrom),'2012-07-01')
#最大连续亏损结束时间
expect_equal(as.character(drawDonwInfo$consecLossTo),'2012-12-01')
