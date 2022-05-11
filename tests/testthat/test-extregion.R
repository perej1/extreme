n <- 1000
mu <- c(1, -1)
sigma <- matrix(c(11, 10.5, 10.5, 11.25), nrow = 2, byrow = TRUE)
x <- relliptical(rgent(n, 2, 10), mu, sigma)

test_that("output is ellipsoidq object", {
  ret <- extregion(x, 10e-3, 10, method = "sample")
  expect_s3_class(extregion(x, 10e-3, 10, method = "sample"), "ellipsoidq")
  expect_s3_class(extregion(x, 10e-3, 10, method = "mcd", alpha = 0.5),
                  "ellipsoidq")
})

test_that("error is thrown for invalid method", {
  expect_error(extregion(x, 10e-3, 10, method = "wrong"), "Invalid 'method'")
  expect_error(extregion(x, 10e-3, 10, method = "mea"), "Invalid 'method'")
})

test_that("error is thrown if method is mcd but alpha is invalid", {
  expect_error(extregion(x, 10e-3, 10, method = "mcd", alpha = NULL),
               "'method == mcd' but value for alpha is not given")
  expect_error(extregion(x, 10e-3, 10, method = "mcd", alpha = "a"))
})
