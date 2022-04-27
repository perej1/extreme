#' Title
#'
#' @param mu
#' @param sigma
#' @param scale
#'
#' @return
#' @export
#'
#' @examples
new_extremellipse <- function(mu, sigma, scale = 1) {
  stopifnot(is.numeric(mu))
  stopifnot(is.matrix(sigma))
  stopifnot(is.numeric(scale))

  structure(list(mu = mu, sigma = sigma, scale = scale),
            class = "extremellipse")
}

validate_extremellipse <- function(x) {
  # No validations yet
  x
}

extremellipse <- function(mu, sigma, scale = 1) {
  validate_extremellipse(new_extremellipse(mu, sigma, scale))
}
