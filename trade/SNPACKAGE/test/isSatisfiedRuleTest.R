require(quantmod)
require(testthat)

# 数据周一涨 周四跌
data = c(1,0,0,-1,0)
# 规则周一涨 周四跌
rule = c(1,0,0,-1,0)
#符合规则
expect_true(isSatisfiedRule(data,rule))

# 数据周一涨 周四跌
data = c(1,0,0,-1,0)
# 规则 周一涨
rule = c(1,0,0,0,0)
#符合规则
expect_true(isSatisfiedRule(data,rule))

#数据 周一涨 周四跌
data = c(1,0,0,-1,0)
#规则 周一涨 周四涨
rule = c(1,0,0,1,0)
#不符合规则
expect_false(isSatisfiedRule(data,rule))
