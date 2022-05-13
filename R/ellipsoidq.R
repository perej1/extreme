new_ellipsoidq <- function(mu = c(-25, 10),
                           scatter = matrix(c(11, 10.5, 10.5, 11.25),
                             byrow = TRUE, ncol = 2
                           ), r = 1) {
  structure(list(mu = mu, scatter = scatter, r = r),
    class = "ellipsoidq"
  )
}

validate_ellipsoidq <- function(x) {
  # No validations yet
  x
}

ellipsoidq <- function(mu, scatter, r) {
  validate_ellipsoidq(new_ellipsoidq(mu, scatter, r))
}

integrate <- function(f, qhat, q, n = 10000) {
  qinv <- solve(q$scatter)
  qhatinv <- solve(qhat$scatter)

  # Define rectangular integration domain
  qradius <- max(1 / sqrt(eigen(qinv)$values))
  qhatradius <- max(1 / sqrt(eigen(qhatinv)$values))
  lower <- pmin(q$mu - qradius, qhat$mu - qhatradius)
  upper <- pmax(q$mu + qradius, qhat$mu + qhatradius)

  # Sample n observations from multivariate uniform
  x <- purrr::map2(lower, upper, ~ runif(n, min = .x, max = .y))
  x <- purrr::map(purrr::transpose(x), purrr::flatten_dbl)

  # Define functions
  fq <- function(x) {
    ifelse(mahalanobis(x, q$mu, qinv, inverted = TRUE) < q$r^2, f(x), 0)
  }

  fqhat <- function(x) {
    ifelse(mahalanobis(x, qhat$mu, qhatinv, inverted = TRUE) < qhat$r^2,
           f(x), 0)
  }

  fqqhat <- function(x) {
    ifelse(mahalanobis(x, q$mu, qinv, inverted = TRUE) < q$r^2 &&
             mahalanobis(x, qhat$mu, qhatinv, inverted = TRUE) < qhat$r^2,
           f(x), 0)
  }

  z1 <- purrr::map_dbl(x, fq)
  z2 <- purrr::map_dbl(x, fqhat)
  z3 <- purrr::map_dbl(x, fqqhat)
  i <- prod(upper - lower)
  c(estimate = i * mean(z1), real = i * mean(z2), overlap = i * mean(z3))
}

mcint <- function(lower, upper, f, n = 10000) {
  # Sample n observations from multivariate uniform
  x <- purrr::map2(lower, upper, ~ runif(n, min = .x, max = .y))
  x <- purrr::map(purrr::transpose(x), purrr::flatten_dbl)

  # Calculate function values at x
  y <- purrr::map_dbl(x, f)

  # Estimate integral
  prod(upper - lower) * mean(y)
}
