#' Estimate Extreme Value Index
#'
#' Use Hill estimator for estimating the extreme value index.
#'
#' Remember that Hill estimator is consistent only for heavy-tailed
#' distributions since by construction the estimate is always positive.
#'
#' @param x Data vector.
#' @param k Threshold parameter for the estimator.
#' @param tail if equal to \code{TRUE}, then it is assumed that \code{x} is an
#'   increasingly ordered sample from the tail, otherwise, it is assumed that
#'   \code{x} is unordered vector including all observations. Also, if
#'   \code{tail = FALSE} then value for the threshold parameter \code{k} has
#'   to be supplied.
#'
#' @return A scalar, estimate for extreme value index.
#' @export
#'
#' @examples
#' TODO
gamma <- function(x, k = NULL, tail = TRUE) {
  if (!tail) {
    n <- length(x)
    x <- sort(x, decreasing = FALSE)
    x <- x[(n - k):n]
  }
  x <- log(x) - log(x[1])
  mean(x[-1])
}
