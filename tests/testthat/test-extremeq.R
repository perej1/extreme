test_that("mcint routine works for constant functions", {
  f <- function(x) 1
  expect_equal(mcint(c(0,0), c(1,1), f, n = 1000), 1)
})
