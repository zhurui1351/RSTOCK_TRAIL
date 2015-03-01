#参数中不存在equity或price 报错
expect_error(totalEquity(ratio=0.1)())
#传入的资金为浮动资金
pos = totalEquity(ratio=0.1)(initeq=10000,price=10)
expect_equal(pos,100)
