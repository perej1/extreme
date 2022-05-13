"%!in%" <- Negate("%in%")

#' Generate Sample from Generating Variate of t Distribution
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

#' Density of Multivariate t Distribution
#'
#' @param x Vector of quantiles.
#' @param mu Location vector.
#' @param sigma Scatter matrix.
#' @param df Degrees of freedom.
#'
#' @return Density at point \code{x}.
#' @noRd
mvtdens <- function(x, mu, sigma, df) {
  d <- length(mu)
  sigmainv <- solve(sigma)
  x <- mahalanobis(x, mu, sigmainv, inverted = TRUE)

  term1 <- gamma((d + df) / 2) / gamma(df / 2)
  term2 <- sqrt(det(sigmainv) / (df * pi) ^ d)
  term3 <- (1 + x / df) ^ (-(d + df) / 2)

  term1 * term2 * term3
}
