#使用IBM数据
require(TTR)
require(testthat)
#使用ATR N=2计算
p = volatilityPositionForTotalEq(IBM,n=2,ratio=0.01)
#n=2 因此IBM数据中第二条数据的atr为na 返回0
pos = p(eq = 100000,tradeDate=time(IBM)[2])
expect_equal(pos,0)

p = volatilityPositionForTotalEq(IBM,n=2,ratio=0.01)
pos = p(eq = 100000,tradeDate=time(IBM)[3])
expect_equal(pos,1169)
