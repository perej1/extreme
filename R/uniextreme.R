#' Estimate Extreme Value Index
#'
#' Use Hill estimator for estimating extreme value index.
#'
#' Remember that Hill estimator is consistent only for heavy-tailed
#' distributions since by construction the estimate is always positive.
#'
#' @param x Data vector.
#' @param k Threshold parameter for the estimator. Value for the parameter has
#'   to be supplied if \code{tail = FALSE}.
#' @param tail if equal to \code{TRUE}, then it is assumed that \code{x} is an
#'   increasingly ordered sample from the tail, otherwise, it is assumed that
#'   \code{x} is an unordered vector including all observations.
#'
#' @return A scalar, estimate for extreme value index.
#' @export
#'
#' @examples
#' TODO
gamma <- function(x, k = NULL, tail = TRUE) {
  if (!tail) {
    n <- length(x)
    x <- sort(x, decreasing = FALSE)[(n - k):n]
  }
  x <- log(x) - log(x[1])
  mean(x[-1])
}

#' Estimate Extreme Quantile for Heavy-Tailed Distributions
#'
#' Estimates \eqn{(1-p)}-quantile of a heavy-tailed distribution for a very
#' small \eqn{p}.
#'
#' Estimator is consistent only for heavy-tailed distributions. Extreme value
#' index is estimated with Hill estimator.
#'
#' @param x Data vector.
#' @param p Probability corresponding to \code{(1-p)}-quantile.
#' @param k Threshold parameter for the estimator. Value for the parameter has
#'   to be supplied if \code{tail=FALSE}.
#' @param n Original sample size. Value has to be supplied if \code{tail=TRUE}.
#' @param tail if equal to \code{TRUE}, then it is assumed that \code{x} is an
#'   increasingly ordered sample from the tail, otherwise, it is assumed that
#'   \code{x} is unordered vector including all observations. Also, if
#'   \code{tail = FALSE} then value for the threshold parameter \code{k} has
#'   to be supplied.
#' @return A scalar, \eqn{(1-p)}-quantile.
#' @export
#'
#' @examples
#' TODO
extq <- function(x, p, k = length(x), n = NULL, tail = TRUE) {
  if (!tail) {
    n <- length(x)
    x <- sort(x, decreasing = FALSE)[(n - k):n]
  }
  x[1] * (k / (n * p))^gamma(x, tail = TRUE)
}
