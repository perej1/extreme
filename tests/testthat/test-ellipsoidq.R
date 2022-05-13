test_that("mcint routine works for constant functions", {
  sigma <- matrix(c(11, 10.5, 10.5, 11.25), nrow = 2, byrow = TRUE)
  mu <- c(-1, 1)
  df <- 10
  d <- length(mu)
  p <- 0.5
  q_radius <- sqrt(d * qf(1 - p, d, df))
  x <- ellipsoidq(mu, sigma, q_radius)
  integrate(function(x) mvtdens(x, mu, sigma, df), x, x, n = 10000)
})
