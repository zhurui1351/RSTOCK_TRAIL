require(quantmod)
require(testthat)
#周三周涨的前提下 周五涨
prerule = c(0,0,1,1,0)
postrule =c(0,0,1,1,1)

rule = getConditionTradeRule(prerule,postrule)

#期望得到规则，周三周四涨的情况下，周五做多

expected_rule = list(rule=prerule,buyday=5,sellday=5,short=FALSE)

expect_equal(expected_rule,rule)
