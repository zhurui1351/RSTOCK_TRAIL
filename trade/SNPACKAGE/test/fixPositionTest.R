require(testthat)
#返回固定头寸
pos = fixPosition(size = 1000)()
expect_equal(pos,1000)
