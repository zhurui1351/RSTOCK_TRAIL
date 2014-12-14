require(quantmod)
require(testthat)

#对星期二的涨跌感兴趣，交易天数为7天
value = getRuleByPos(c(2),tradeDays = 7)
#星期二的位置上被设置为1和-1
expected_value1 = c(0,1,0,0,0,0,0)
expected_value2 = c(0,-1,0,0,0,0,0)

expect_identical(expected_value1,value[[1]])
expect_identical(expected_value2,value[[2]])

#对周四周五的涨跌感兴趣，交易天数为5天
value = getRuleByPos(c(4,5),tradeDays = 5)
#共有四种可能
expected_value1 = c(0,0,0,1,1)
expected_value2 = c(0,0,0,1,-1)
expected_value3 = c(0,0,0,-1,1)
expected_value4 = c(0,0,0,-1,-1)


expect_identical(expected_value1,value[[1]])
expect_identical(expected_value2,value[[2]])
expect_identical(expected_value3,value[[3]])
expect_identical(expected_value4,value[[4]])
