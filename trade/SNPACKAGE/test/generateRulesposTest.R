require(quantmod)
require(testthat)

#两个位置的排列
result = generateRulespos(c(1,2))

#应该产生所有的子集
expected_result = list(c(1),c(2),c(1,2))

#断言相等
expect_identical(result,expected_result)
