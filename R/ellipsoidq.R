#' Helper for Creating Objects of Class \code{ellipsoidq}
#'
#' Creates an object that represents a quantile region or estimated quantile
#' region of an elliptical distribution.
#'
#' Object of class \code{ellipsoidq} represents complement of an open
#' ellipsoid,
#' \deqn{Q(\mu, \Sigma, r) \coloneqq
#' \{x\in\mathbb{R}^d : (x - \mu)^T\Sigma^{-1}(x - \mu) \geq r^2\}.}
#' Here \eqn{\mu}, \eqn{\Sigma} and \eqn{r} correspond to \code{mu},
#' \code{Sigma} and \code{scale}, respectively. For example,
#' \eqn{Q(\mu, \Sigma, r)} is the \eqn{(1-p)} quantile region of an elliptical
#' distribution when \eqn{\mu} is equal to the location of the elliptical
#' distribution, \eqn{\Sigma} is equal to the scatter matrix of the distribution
#' and \eqn{r} is the \eqn{(1-p)}-quantile of the generating variate
#' \eqn{\mathcal{R}}. See documentation of \code{\link{relliptical}} for more
#' details about elliptical distributions.
#'
#' @seealso \code{\link{relliptical}}, \code{\link{qreg}}
#'
#' @param mu An integer or double vector. Represents location of the ellipsoid.
#' @param sigma An integer or double matrix. Determines the shape of the
#'   ellipsoid.
#' @param scale An integer or double scalar. Scales the ellipsoid, see details
#'   for more information.
#'
#' @return An object of class \code{ellipsoidq}. Object of class
#'   \code{ellipsoidq} is a list containing values of \code{mu}, \code{sigma}
#'   and \code{scale}.
#'
#' @export
#' @examples
#' # (1 - 0.05) quantile region a of 2-dimensional t-distribution with degrees
#' # of freedom equal to three.
#' p <- 0.05
#' d <- 2
#' df <- 3
#' mu <- c(-1, 1)
#' sigma <- matrix(c(11, 10.5, 10.5, 11.25), nrow = 2, byrow = TRUE)
#' r <- sqrt(d * qf(1 - p, d, df))
#' x <- ellipsoidq(mu, sigma, r)
ellipsoidq <- function(mu = rep(0, 2), sigma = diag(2), scale = 1) {
  validate_ellipsoidq(new_ellipsoidq(mu, sigma, scale))
}

#' Constructor for Objects of Class ellipsoidq
#'
#' Constructs ellipsoidq object without any validation checks.
#'
#' @param mu An integer or double vector. Represents location of the ellipsoid.
#' @param sigma An integer or double matrix. Determines the shape of the
#'   ellipsoid.
#' @param scale An integer or double scalar. Scales the ellipsoid.
#'
#' @return An ellipsoidq object.
#' @noRd
new_ellipsoidq <- function(location = rep(0, 2), scatter = diag(2), scale = 1) {
  structure(list(location = location, scatter = scatter, scale = scale),
    class = "ellipsoidq"
  )
}

#' Validity Checks for ellipsoidq
#'
#' Checks that input arguments for generating new ellipsoidq object are valid.
#'
#' @param x An ellipsoidq object.
#'
#' @return An ellipsoidq object.
#' @noRd
validate_ellipsoidq <- function(x) {
  values <- unclass(x)
  if (!is_numeric(values$location)) {
    abort("`location` must be an integer or double vector.")
  }
  if (!is_matrix(values$scatter)) {
    abort("`scatter` must be an integer or double matrix.")
  }
  if (!is_scalar_numeric(values$scale) || values$scale <= 0) {
    abort("`scale` must be a positive integer or double of length 1.")
  }
  if (!all(dim(values$scatter) == rep(length(values$location), 2))) {
    abort("Dimensions of `location` and `scatter` must match.")
  }
  x
}
