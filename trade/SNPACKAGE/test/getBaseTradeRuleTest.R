require(quantmod)
require(testthat)
#构造基础规则
#周一涨 周三跌
baserule = c(1,0,-1,0,0)

rule = getBaseTradeRule(baserule)

#期望输入，前提规则周一涨c(1,0,0,0,0),周三买卖，做空

expected_rule = list(rule=c(1,0,0,0,0),buyday=3,sellday=3,short=TRUE)

expect_equal(expected_rule,rule)
