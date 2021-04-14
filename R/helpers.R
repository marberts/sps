# None of these functions are exported
#---- Argument checking ----
is_positive_numeric <- function(x) {
  is.numeric(x) && all(is.finite(x)) && all(x > 0)
}

is_positive_number <- function(x) {
  length(x == 1) && is_positive_numeric(x)
}

is_positive_number1 <- function(x) {
  is_positive_number(x) && x >= 1
}

#---- Inclusion probability ----
incl_prob <- function(x, n) {
  x / sum(x) * n
}
