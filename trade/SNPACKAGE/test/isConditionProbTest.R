require(quantmod)
require(testthat)
#构造条件 r1 周一涨 周二跌 周三涨
#r2 周一涨 周二跌
r1 = c(1,-1,1,0,0)
r2 = c(1,-1,0,0,0)
#可以合并
expect_true(isConditionProb(r1,r2))

#r1 周一周二涨 r2周一涨 周二跌
r1 = c(1,1,0,0,0)
r2 = c(1,-1,0,0,0)
#无法合并
expect_false(isConditionProb(r1,r2))

#r1周一涨 周二涨 周四涨
#r2 周一涨 周二跌
r1 = c(1,1,0,1,0)
r2 = c(1,-1,0,0,0)
#无法合并
expect_false(isConditionProb(r1,r2))


