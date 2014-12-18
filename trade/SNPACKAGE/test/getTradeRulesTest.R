require(quantmod)
require(testthat)

#构造基础概率,周一涨
base1 = list(rule=c(1,0,0,0,0),ratio=0.5,num_satisfied = 400,numweek=800,type="base",pvalue=0.5)
#构造条件概率周一涨的情况下周二跌
condition1 = list(pre=c(1,0,0,0,0),cons=c(1,-1,0,0,0),prob=0.5,numpre=400,numcons=200,type="condition",pvalue=0.5)
probs = list(base1,condition1)

result = getTradeRules(probs)

#期望得到的输出
#条件c(0,0,0,0,0)表示每周都符合要求，周一做多
baseTradeRule = list(rule=c(0,0,0,0,0),buyday=1,sellday=1,short=FALSE)
expect_equal(list(probrule=base1,traderule=baseTradeRule),result[[1]])
#周一涨的情况下，周二做空
conditionTradeRule = list(rule=c(1,0,0,0,0),buyday=2,sellday=2,short=TRUE)
expect_equal(list(probrule=condition1,traderule=conditionTradeRule),result[[2]])
