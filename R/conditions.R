is_numeric <- function(x) {
  is_integer(x) || is_double(x)
}

is_matrix <- function(x) {
  is_numeric(x) && is.matrix(x)
}

is_scalar_numeric <- function(x) {
  is_scalar_integer(x) || is_scalar_double(x)
}
