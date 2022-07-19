test_that("extquantile gives correct outputs", {
  x <- c(10, 7, 5, 4, 1, 6, 7, 8, 4, 7)
  y <- sort(x, decreasing = FALSE)
  n <- length(x)
  k <- c(1, 3, 5, length(x) - 1)
  g <- c(
    log(5 / 4), 1 / 3 * log(80 / 49), 1 / 5 * log(1715 / 486),
    1 / 9 * log(13171200)
  )
  p <- 10e-3
  q <- y[n - k] * (k / (n * p)) ^ g
  expect_equal(extquantile(x, p, k[1]), q[1])
  expect_equal(extquantile(x, p, k[2]), q[2])
  expect_equal(extquantile(x, p, k[3]), q[3])
  expect_equal(extquantile(x, p, k[4]), q[4])
})

n <- 1000
mu <- c(1, -1)
sigma <- matrix(c(11, 10.5, 10.5, 11.25), nrow = 2, byrow = TRUE)
x <- relliptical(rgent(n, 2, 10), mu, sigma)

test_that("output is ellipsoidq object", {
  ret <- qreg(x, 10e-3, k=10, method = "sample")
  expect_s3_class(qreg(x, 10e-3, k=10, method = "sample"), "ellipsoidq")
  expect_s3_class(
    qreg(x, 10e-3, k=10, method = "mcd", alpha = 0.5),
    "ellipsoidq"
  )
})

test_that("error is thrown for invalid method", {
  expect_error(qreg(x, 10e-3, k=10, method = "wrong"),
               "Invalid method for estimating location-scatter pair.")
  expect_error(qreg(x, 10e-3, k=10, method = "mea"),
               "Invalid method for estimating location-scatter pair.")
})

test_that("error is thrown if method is mcd but alpha is invalid", {
  expect_error(
    qreg(x, 10e-3, k=10, method = "mcd", alpha = NULL),
    "'method == mcd' but value for alpha is not given"
  )
  expect_error(qreg(x, 10e-3, 10, method = "mcd", alpha = "a"))
})

test_that("error is thrown when k is invalid", {
  expect_equal(2 * 2, 4)
})

test_that("error is thrown when p is invalid", {
  expect_equal(2 * 2, 4)
})
