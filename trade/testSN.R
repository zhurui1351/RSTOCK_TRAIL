library(testthat)
a <- 11
expect_that(a, is_less_than(10))
expect_less_than(a, 10)
