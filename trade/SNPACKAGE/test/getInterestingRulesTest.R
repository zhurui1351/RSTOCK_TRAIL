require(quantmod)
require(testthat)
#构造基础概率
base1 = list(rule=c(1,0,0,0,0),ratio=0.5,num_satisfied = 400,numweek=800)
#构造条件概率
condition1 = list(pre=c(1,0,0,0,0),cons=c(1,1,0,0,0),prob=0.5,numpre=400,numcons=200)

baseProbs = list(base1)
conditionProbs = list(condition1)
#概率大于0.6 pvalue大于0.05
rules = getInterestingRules(0.6,0.05,baseProbs,conditionProbs)
#判断返回值是否符合要求
expect_equal(list(),rules)

#概率大于0.5 pvalue大于0.05
rules = getInterestingRules(0.5,0.05,baseProbs,conditionProbs)

#期望的返回值
expected_rule1 = list(rule=c(1,0,0,0,0),ratio=0.5,num_satisfied = 400,numweek=800,type="base",pvalue=0.5)

expected_rule2 = list(pre=c(1,0,0,0,0),cons=c(1,1,0,0,0),prob=0.5,numpre=400,numcons=200,type="condition",pvalue=0.5)


#断言
expect_equal(expected_rule1,rules[[1]])
expect_equal(expected_rule2,rules[[2]])


