require(quantmod)
require(testthat)

#假设交易天数为2天，对所有的规则感兴趣
rule = generateRules(prune = 2,tradeDays = 2)

# 交易只有两天的情况下，共有8条规则产生
expect_equal(8,length(rule))
#共有八种组合
v1 = c(1,1)
v2 = c(1,-1)
v3 = c(-1,1)
v4 = c(-1,-1)
v5 = c(0,1)
v6 = c(0,-1)
v7 = c(1,0)
v8 = c(-1,0)
#断言相等
expect_identical(v1,rule[[1]])
expect_identical(v2,rule[[2]])
expect_identical(v3,rule[[3]])
expect_identical(v4,rule[[4]])
expect_identical(v5,rule[[5]])
expect_identical(v6,rule[[6]])
expect_identical(v7,rule[[7]])
expect_identical(v8,rule[[8]])
