#参数中不存在equity或price 报错
expect_error(CoreEquity(ratio=0.1)())
#初始金为10000，分配比例为0.1
pos = CoreEquity(ratio=0.1)(eq=10000,price=10)
expect_equal(pos,100)
