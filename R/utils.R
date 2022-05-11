"%!in%" <- Negate("%in%")

#' Generate sample from generating variate of t-distribution
#'
#' @param n Sample size.
#' @param d Dimensions of t-distribution.
#' @param df Degrees of freedom.
#'
#' @return A vector of size \code{n} representing sample from generating
#'   variate of t-distribution.
#' @noRd
rgent <- function(n, d, df){
  sqrt(d * stats::rf(n, d, df))
}
