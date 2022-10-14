# None of these functions are exported
#---- Argument checking ----
not_positive_vector <- function(x) {
  !(length(x) > 0 && is.numeric(x) && all(is.finite(x)) && all(x >= 0))
}

not_strict_positive_vector <- function(x) {
  !(length(x) > 0 && is.numeric(x) && all(is.finite(x)) && all(x > 0))
}

not_positive_number <- function(x) {
  length(x) != 1L || not_positive_vector(x)
}

not_prob <- function(x) {
  not_strict_positive_vector(x) || any(x >= 1)
}
