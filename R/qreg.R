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
#' @noRd
extquantile <- function(x, p, k) {
  n <- length(x)
  if (p <= 0 || p >= 1) abort("p must be a value such that 0 < p < 1")
  if (!(k %in% 1:(n - 1))) {
    abort("value for k has to a whole number between 1 and n-1")
  }
  x <- sort(x, decreasing = FALSE)[(n - k):n]
  gamma <- mean((log(x) - log(x[1]))[-1])
  x[1] * (k / (n * p))^gamma
}

#' Estimate Quantile Region
#'
#' Estimates (1-p)-quantile region.
#'
#' Two estimators are provided. Extreme quantile estimator is consistent only
#' for heavy-tailed elliptical distributions. Extreme value index is estimated
#' with separating Hill estimator. Other estimator is the conventional sample
#' quantile.
#'
#' @param x A matrix representing sample from elliptical distribution.
#'   Rows are observations and columns dimensions.
#' @param p Probability corresponding to \code{(1-p)}-quantile. Values in
#'   interval \eqn{(0, 1)} are accepted.
#' @param method How location and scatter should be estimated?
#' @param qmethod How quantile should be estimated?
#' @param k Threshold parameter for the estimator. Threshold parameter has to
#'   be in set \eqn{{1,2,\ldots,n}} where \eqn{n} is equal to \code{nrow(x)}.
#' @param alpha Controls size of the subsets over which the determinant is
#'   minimized when \code{method = "mcd"}.
#' @param mu Given estimate for location.
#' @param sigma Given estimate for scatter.
#'
#' @return A ellipsoidq object representing the estimate.
#' @export
#'
#' @examples
#' #TODO
qreg <- function(x, p, method = "sample", qmethod = "sample", k = NULL,
                 alpha = NULL, mu = NULL, sigma = NULL) {
  if (!(method %in% c("sample", "mcd", "given"))) {
    abort("Invalid method for estimating location-scatter pair.")
  }
  if (!(qmethod %in% c("extreme", "sample"))) {
    abort("Invalid method for estimating quantile.")
  }
  if (method == "mcd" && is.null(alpha)) {
    abort("'method == mcd' but value for alpha is not given.")
  }
  if (method == "given" && (is.null(mu) || is.null(sigma))) {
    abort("'method == given' but value for mu or sigma is not given.")
  }
  if (method == "sample") {
    mu <- colMeans(x)
    sigma <- stats::cov(x)
  }
  if (method == "mcd") {
    est <- robustbase::covMcd(x, alpha = alpha)
    mu <- est$center
    sigma <- est$cov
  }

  x <- sqrt(stats::mahalanobis(x, mu, sigma, inverted = FALSE))
  if (qmethod == "extreme") {
    r <- extquantile(x, p, k)
    }
  if (qmethod == "sample") {
    r <- stats::quantile(x, 1 - p)
  }
  ellipsoidq(mu, sigma, r)
}
