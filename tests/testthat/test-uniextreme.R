x <- c(10, 7, 5, 4, 1, 6, 7, 8, 4, 7)
y <- sort(x, decreasing = FALSE)
n <- length(x)
k <- c(1, 3, 5, length(x) - 1)
g <- c(log(5 / 4), 1 / 3 * log(80 / 49), 1 / 5 * log(1715 / 486),
       1 / 9 * log(13171200))
p <- 10e-3
q <- y[n-k] * (k / (n * p)) ^ g

test_that("extquantile gives correct outputs", {
  expect_equal(extquantile(x, p, k[1]), q[1])
  expect_equal(extquantile(x, p, k[2]), q[2])
  expect_equal(extquantile(x, p, k[3]), q[3])
  expect_equal(extquantile(x, p, k[4]), q[4])
})

test_that("error is thrown when k is invalid", {
  err_message <- "value for k has to a whole number between 1 and n-1"
  expect_error(extquantile(x, p, 2.5), err_message)
  expect_error(extquantile(x, p, -1), err_message)
  expect_error(extquantile(x, p, 0), err_message)
  expect_error(extquantile(x, p, length(x)), err_message)
})

test_that("error is thrown when p is invalid", {
  err_message <- "p must be a value such that 0 < p < 1"
  expect_error(extquantile(x, 0, k[1]), err_message)
  expect_error(extquantile(x, 1, k[1]), err_message)
  expect_error(extquantile(x, -1, k[1]), err_message)
  expect_error(extquantile(x, 1.1, k[1]), err_message)
})
