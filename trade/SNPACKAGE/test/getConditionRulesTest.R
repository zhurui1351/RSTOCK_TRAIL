require(quantmod)
require(testthat)

#构造基础概率，rule是规则,ratio是规则在历史中出现的频率，num_satisfied是规则
#在历史中被满足的个数 numweek是总共的测试周数
base1 = list(rule=c(1,0,0,0,0),ratio=0.5,num_satisfied = 4,numweek=8)
base2 = list(rule=c(1,1,0,0,0),ratio=0.25,num_satisfied = 2,numweek=8)
base3 = list(rule=c(1,1,1,0,0),ratio=0.125,num_satisfied = 1,numweek=8)

baseProbs = list(base1,base2,base3)

conditionProbs = getConditionRules(baseProbs)
#期望两个条件概率
expected_condition1 = list(pre=c(1,0,0,0,0),cons=c(1,1,0,0,0),prob=0.5,numpre=4,numcons=2)
expected_condition2 = list(pre=c(1,1,0,0,0),cons=c(1,1,1,0,0),prob=0.5,numpre=2,numcons=1)
#判断返回值是否符合要求
expect_equal(expected_condition1,conditionProbs[[1]])
expect_equal(expected_condition2,conditionProbs[[2]])
