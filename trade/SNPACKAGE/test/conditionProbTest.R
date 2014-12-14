require(quantmod)
require(testthat)

#构造数据，base1出现2次，base2出现1次
base1 = list(rule=c(1,0,0,0,0),ratio=0.5,num_satisfied = 2,numweek=4)
base2 = list(rule=c(1,1,0,0,0),ratio=0.25,num_satisfied = 1,numweek=4)

condition = conditionProb(base1,base2)

#期望的输出值，前验规则是c(1,0,0,0,0)，后验规则是c(1,1,0,0,0),频率是0.5
expected_condition = list(pre=c(1,0,0,0,0),cons=c(1,1,0,0,0),prob=0.5,numpre=2,numcons=1)

#断言相等
expect_equal(condition,expected_condition)
