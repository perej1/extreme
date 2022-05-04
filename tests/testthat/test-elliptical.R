test_that("dimensions of square root are right", {
  sigma <- matrix(c(1, 1, 0.5, 1), nrow = 2, byrow = TRUE)
  lambda <- sqrtmat(sigma)
  expect_equal(dim(lambda), dim(sigma))
})
