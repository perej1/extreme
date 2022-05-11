extregion <- function(x, p, k, method = "sample", alpha = NULL) {
  if (method %!in% c("sample", "mcd")) {
    abort("Invalid 'method'")
  }
  if (method == "mcd" && is.null(alpha)) {
    abort("'method == mcd' but value for alpha is not given")
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
  r <- extquantile(x, p, k)
  ellipsoidq(mu, sigma, r)
}
