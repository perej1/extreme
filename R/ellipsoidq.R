ellipsoidq <- function(location = rep(0, 2), scatter = diag(2), scale = 1) {
  validate_ellipsoidq(new_ellipsoidq(location, scatter, scale))
}

new_ellipsoidq <- function(location = rep(0, 2), scatter = diag(2), scale = 1) {
  structure(list(location = location, scatter = scatter, scale = scale),
    class = "ellipsoidq"
  )
}

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
