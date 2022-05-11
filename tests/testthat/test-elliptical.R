sigma1 <- matrix(c(11, 10.5, 10.5, 11.25), nrow = 2, byrow = TRUE)
sigma2 <- matrix(c(2, -1, 0, -1, 2, -1, 0, -1, 2), nrow = 3, byrow = TRUE)
sigma <- matrix(c(1, 2, 2, 1), byrow = T, nrow = 2) # not posdef

mu <- c(1, -1)

lambda1 <- sqrtmat(sigma1)
lambda2 <- sqrtmat(sigma2)

test_that("dimensions of square root are right", {
  expect_equal(dim(lambda1), dim(sigma1))
  expect_equal(dim(lambda2), dim(sigma2))
})

test_that("square root is symmetric", {
  expect_equal(lambda1, t(lambda1))
  expect_equal(lambda2, t(lambda2))
})

test_that("square root times square root is equal to original", {
  expect_equal(lambda1 %*% lambda1, sigma1)
  expect_equal(lambda2 %*% lambda2, sigma2)
})

test_that("error is thrown for matrices that are not positive definite", {
  expect_error(sqrtmat(sigma), "Scatter matrix is not positive definite")
})

test_that("output of relliptical has right dimensions", {
  n <- 4
  d <- 2
  x <- rgent(n, d, 1)
  expect_equal(dim(relliptical(x, mu, sigma1)), c(n, d))
})
