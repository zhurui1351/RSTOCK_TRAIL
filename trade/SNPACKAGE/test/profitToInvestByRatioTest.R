require(testthat)
#参数中不存在equity或price 报错
expect_error(profitToInvestByRatio(ratio=0.1,profitratio = 0.1)())
#初始金为10000，分配比例为0.1,利润增长0.1后投入本金,当前亏损，使用eq分配
pos = profitToInvestByRatio(ratio=0.1,profitratio = 0.1)(eq=10000,initeq=11000,price=10)
expect_equal(pos,100)

#产生利润后投入使用

pos = profitToInvestByRatio(ratio=0.1,profitratio = 0.1)(eq=13000,initeq=10000,price=10)
expect_equal(pos,157)
