#' Calculate symmetric square root of a symmetric positive definite matrix
#' Returns a matrix \eqn{\Lambda} such that \eqn{\Lambda \Lambda = \Sigma}.
#' Matrix \eqn{\Lambda} is symmetric, i.e, \eqn{\Lambda^T = \Lambda}.
#'
#' @param sigma Scatter matrix.
#' @return A matrix.
#' @noRd
sqrtmat <- function(sigma) {
  eval <- eigen(sigma)$values
  if (!all(eval > 0)) abort("Scatter matrix is not positive definite")
  evec <- eigen(sigma)$vectors
  evec %*% diag(eval ^ (1 / 2)) %*% t(evec)
}

#' Generate Observation from Elliptical Distribution
#'
#' This is a helper function for sampling from elliptical distributions.
#'
#' @param r Observation sampled from generating variate.
#' @param mu Location vector.
#' @param lambda Square root of the scatter matrix.
#'
#' @return A vector of length \code{length(mu)} representing one observation.
#' @noRd
pull_elliptical <- function(r, mu, lambda) {
  d <- length(mu)

  # Generate observation from uniform distribution on unit sphere
  u <- MASS::mvrnorm(1, rep(0, d), diag(d))
  u <- u / norm(u, type = "2")

  as.vector(mu + r * lambda %*% u)
}

#' Simulate from an Elliptical Distribution
#'
#' Generate a sample from an elliptical distribution.
#'
#' Samples from elliptical distribution with location \code{mu}, scatter matrix
#' \code{sigma} and generating variate specified by the sample x. Size of the
#' sample is \code{length(x)}.
#'
#' @param x Sample from generating variate.
#' @param mu Location vector.
#' @param sigma Positive-definite scatter matrix.
#' @return An \code{length(x)} times \code{length(mu)} matrix with one
#'   observation in each row.
#' @export
#'
#' @examples
#' #TODO
relliptical <- function(x, mu, sigma) {
  if (!all(dim(sigma) == rep(length(mu), 2))) {
    abort("Dimensions of mu and sigma do not match")
  }
  x <- purrr::map(x, ~ pull_elliptical(.x, mu, sqrtmat(sigma)))
  matrix(purrr::flatten_dbl(x),
    nrow = length(x),
    ncol = length(mu),
    byrow = TRUE
  )
}
