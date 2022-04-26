#' Estimate Extreme Value Index
#'
#' Use Hill estimator for estimating the extreme value index.
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
