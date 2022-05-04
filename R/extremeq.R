new_extremeq <- function(mu = c(-25, 10),
                         scatter = matrix(c(11, 10.5, 10.5, 11.25),
                                          byrow = TRUE, ncol = 2)) {
  stopifnot(is.numeric(mu))
  stopifnot(is.matrix(scatter))

  structure(list(mu = mu, scatter = scatter),
            class = "extremellipse")
}

validate_extremeq <- function(x) {
  # No validations yet
  x
}

extremeq <- function(mu, scatter) {
  validate_extremeq(new_extremeq(mu, scatter))
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
