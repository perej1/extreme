#' Simulate Sample from an Elliptical Distribution
#'
#' Generate a sample from an elliptical distribution with location \code{mu},
#' scatter matrix \code{sigma} and generating variate specified by the sample
#' \code{x}. Size of the generated sample is \code{length(x)}.
#'
#' Function samples from a random variable \eqn{X} with stochastic
#' representation
#' \deqn{X \stackrel{d}{=} \mu + \mathcal{R} \Lambda \mathcal{U},}
#' where \eqn{\mu\in\mathbb{R}^m}, \eqn{\mathcal{R}} is is nonnegative random
#' variable, \eqn{\mathcal{U}} is a \eqn{m}-dimensional random vector uniformly
#' distributed on a unit sphere and \eqn{\Lambda\in \mathbb{R}^{m\times m}} is
#' a matrix such that \eqn{\Sigma = \Lambda\Lambda^T} is a symmetric
#' positive-definite matrix. Random variables \eqn{\mathcal{R}} and
#' \eqn{\mathcal{U}} are independent.
#'
#' @param x A double or integer vector representing a sample from the generating
#'   variate.
#' @param mu A double or integer vector representing location of the
#'   distribution.
#' @param sigma A double or integer matrix representing the scatter matrix of
#'   the distributrion. Argument \code{sigma} must be symmetric
#'   positive-definite scatter matrix.
#' @return An \code{length(x)} times \code{length(mu)} matrix with one
#'   observation in each row.
#' @export
#'
#' @examples
#' # Simulate sample from 3-dimensional t-distribution with degrees of
#' # freedom equal to three.
relliptical <- function(x, mu, sigma) {
  if ((!is.integer(x) && !is.double(x)) || !is.vector(x) || !all(x >= 0)) {
    abort("`x` must be a nonnegative numeric vector.")
  }
  if ((!is.integer(mu) && !is.double(mu)) || !is.vector(mu)) {
    abort("`mu` must be a numeric vector.")
  }
  if ((!is.integer(sigma) && !is.double(sigma)) || !is.matrix(sigma)) {
    abort("`sigma` must be a numeric matrix.")
  }
  if (!all(dim(sigma) == rep(length(mu), 2))) {
    abort("Dimensions of `mu` and `sigma` must match.")
  }
  x <- purrr::map(x, ~ pull_elliptical(.x, mu, sqrtmat(sigma)))
  matrix(purrr::flatten_dbl(x),
    nrow = length(x),
    ncol = length(mu),
    byrow = TRUE
  )
}

#' Calculate Symmetric Square Root of a Symmetric Positive Definite Matrix
#'
#' Returns a symmetric matrix \eqn{\Lambda} such that
#' \eqn{\Lambda \Lambda = \Sigma}.
#'
#' @param sigma A numeric matrix.
#' @return A numeric matrix.
#' @noRd
sqrtmat <- function(sigma) {
  eigenval <- eigen(sigma)$values
  if (any(eigenval <= 0) || any(sigma != t(sigma))) {
    abort("Scatter matrix is not symmetric positive definite")
  }
  eigenvec <- eigen(sigma)$vectors
  eigenvec %*% diag(eigenval^0.5) %*% t(eigenvec)
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
