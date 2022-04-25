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
  u <- rmvnorm(n, rep(0, d), diag(d))
  u <- u / norm(u, type = "2")

  matrix(mu, nrow = n, ncol = d, byrow = TRUE) + r * u %*% t(lambda)
}
