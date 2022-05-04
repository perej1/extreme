#' Calculate symmetric square root of a symmetric positive definite matrix
#'
#' Returns a matrix \eqn{\Lambda} such that \eqn{\Lambda \Lambda = \Sigma}.
#' Matrix \eqn{\Lambda} is symmetric, i.e, \eqn{\Lambda^T = \Lambda}.
#'
#' @param sigma Scatter matrix.
#' @return A matrix.
#' @export
#'
#' @examples
#' x <- matrix(c(1, 1, 0.5, 1), nrow = 2, byrow = TRUE)
#' sqrtmat(x)
sqrtmat <- function(sigma) {
  val <- eigen(sigma)$values
  vec <- eigen(sigma)$vectors
  vec %*% diag(val^(1 / 2)) %*% t(vec)
}


#' Simulate from an Elliptical Distribution
#'
#' Generate a sample from an elliptical distribution.
#'
#' @param r Sample from a generating variate. Also specifies number
#'   of observations.
#' @param mu Location vector.
#' @param lambda A Matrix such that \code{lambda \%*\% lambda == sigma}, where
#'   sigma is a positive-definite scatter matrix.
#' @return An \code{length(r)} times \code{length(mu)} matrix with one sample
#'   in each row.
#' @export
#'
#' @examples
#' TODO
relliptical <- function(r, mu, lambda) {
  d <- length(mu)
  n <- length(r)

  # Generate sample from uniform distribution on unit sphere
  u <- MASS::mvrnorm(n, rep(0, d), diag(d))
  u <- u / norm(u, type = "2")

  matrix(mu, nrow = n, ncol = d, byrow = TRUE) + r * u %*% t(lambda)
}
