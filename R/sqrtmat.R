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
