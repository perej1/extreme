#' Estimate Extreme Quantile for Heavy-Tailed Distributions
#'
#' Estimates \eqn{(1-p)}-quantile of a heavy-tailed distribution for a very
#' small \eqn{p}.
#'
#' Estimator is consistent only for heavy-tailed distributions. Extreme value
#' index is estimated with Hill estimator.
#'
#' @param x Numeric vector. Represents a sample from a heavy-tailed
#'   distribution.
#' @param p Probability corresponding to \code{(1-p)}-quantile. Values in
#'   interval \eqn{(0, 1)} are accepted.
#' @param k Threshold parameter for the estimator. Threshold parameter has to
#'   be in set \eqn{{1,2,\ldots,n}} where \eqn{n} is equal to \code{length(x)}.
#' @return A scalar, representing estimate of \eqn{(1-p)}-quantile.
#' @export
#'
#' @examples
#' TODO
extquantile <- function(x, p, k) {
  n <- length(x)
  if (p <= 0 || p >= 1) abort("p must be a value such that 0 < p < 1")
  if (k %!in% 1:(n - 1)) {
    abort("value for k has to a whole number between 1 and n-1")
  }
  x <- sort(x, decreasing = FALSE)[(n - k):n]
  gamma <- mean((log(x) - log(x[1]))[-1])
  x[1] * (k / (n * p))^gamma
}
