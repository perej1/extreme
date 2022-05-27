#' Simulate a Sample from an Elliptical Distribution
#'
#' Generate a sample from an elliptical distribution with location \code{mu},
#' scatter matrix \code{sigma} and generating variate specified by the sample
#' \code{r}. Size of the generated sample is \code{length(r)}.
#'
#' Function samples from a random variable \eqn{X} with stochastic
#' representation
#' \deqn{X \stackrel{d}{=} \mu + \mathcal{R} \Lambda \mathcal{U},}
#' where \eqn{\mu\in\mathbb{R}^m}, \eqn{\mathcal{R}} is is nonnegative random
#' variable, \eqn{\mathcal{U}} is a \eqn{m}-dimensional random vector uniformly
#' distributed on a unit sphere and \eqn{\Lambda\in \mathbb{R}^{m\times m}} is
#' a matrix such that \eqn{\Sigma = \Lambda\Lambda^T} is a symmetric
#' positive-definite matrix. Random variables \eqn{\mathcal{R}} and
#' \eqn{\mathcal{U}} are independent. See, for example,
#' \insertCite{cambanis1981}{extreme} for more information about elliptical
#' distributions.
#'
#' Matrix \eqn{\Lambda} is calculated with help of the eigenvalue
#' decomposition. I.e,
#' \deqn{\Lambda = U A^{1/2} U^T,}
#' where \eqn{U} is a orthogonal matrix with eigenvectors of the matrix
#' \eqn{\Sigma} as columns and \eqn{A^{1/2} = \textrm{diag}(\lambda^{1/2}_1,
#' \lambda^{1/2}_2, \dots, \lambda^{1/2}_m)}. Here \eqn{\lambda_1, \lambda_2,
#' \ldots, \lambda_m} are eigenvalues of the matrix \eqn{\Sigma}.
#'
#' @param r A double or integer vector representing a sample from the
#'   generating variate.
#' @param mu A double or integer vector representing location of the
#'   distribution.
#' @param sigma A double or integer matrix representing the scatter matrix of
#'   the distribution. Argument \code{sigma} must be symmetric
#'   positive-definite scatter matrix.
#' @return An \code{length(r)} times \code{length(mu)} matrix with one
#'   observation in each row.
#'
#' @examples
#' # Simulate a sample from 2-dimensional t-distribution with degrees of
#' # freedom equal to three
#' n <- 100
#' d <- 2
#' df <- 3
#' r <- sqrt(d * rf(n, d, df))
#' mu <- c(-1, 1)
#' sigma <- matrix(c(11, 10.5, 10.5, 11.25), nrow = 2, byrow = TRUE)
#' x <- relliptical(r, mu, sigma)
#'
#' # Plot the sample
#' plot(x)
#'
#' @references
#'   \insertAllCited{}
#'
#' @export
relliptical <- function(r, mu, sigma) {
  if ((!is.integer(r) && !is.double(r)) || !is.vector(r) || !all(r >= 0)) {
    abort("`r` must be a nonnegative numeric vector.")
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

  r <- purrr::map(r, ~ pull_elliptical(.x, mu, sqrtmat(sigma)))
  matrix(
    purrr::flatten_dbl(r),
    nrow = length(r),
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
