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
#' @param k Threshold parameter for the estimator.
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
extq <- function(x, p, k = NULL, tail = TRUE) {
  # TODO
}
