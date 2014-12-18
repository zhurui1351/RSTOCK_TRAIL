require(quantmod)
require(testthat)

#构造数据,加载xts包中的sample_matrix
data(sample_matrix)

mydata = as.xts(sample_matrix)
#运行周内sn算法,概率大于0.5 pvalue大于0.05 且规则长度只有1的规则
result = SNweeklyRules(mydata,confProp=0.5,conf=0.05,prune=5)
#期望找到周五跌的规则
expected_result = list(rule=c(0,0,0,0,-1),ratio=0.5769231,num_satisfied = 30,numweek=52,type="base",pvalue = 0.8341575 )
#断言
expect_equal(expected_result,result[[1]],tolerance=.002)
