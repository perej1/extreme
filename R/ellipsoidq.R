new_ellipsoidq <- function(mu = c(-25, 10),
                           scatter = matrix(c(11, 10.5, 10.5, 11.25),
                             byrow = TRUE, ncol = 2
                           ), r = 1) {
  structure(list(mu = mu, scatter = scatter, r = r),
    class = "ellipsoidq"
  )
}

validate_ellipsoidq <- function(x) {
  # No validations yet
  x
}

ellipsoidq <- function(mu, scatter, r) {
  validate_ellipsoidq(new_ellipsoidq(mu, scatter, r))
}
