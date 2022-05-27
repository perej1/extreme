is_numeric <- function(x) {
  is_integer(x) || is_double(x)
}

is_matrix <- function(x) {
  is_numeric(x) && is.matrix(x)
}
