# None of these functions are exported
#---- Argument checking ----
is_positive_numeric <- function(x) {
  is.numeric(x) && all(is.finite(x)) && all(x >= 0)
}

is_positive_numeric1 <- function(x) {
  is_positive_numeric(x) && all(x >= 1)
}

is_positive_number <- function(x) {
  length(x == 1) && is_positive_numeric(x)
}

is_positive_number1 <- function(x) {
  is_positive_number(x) && x >= 1
}

#---- Inclusion probability ----
inclusion_prob <- function(x, n) {
  x / sum(x) * n
}

#---- Random rounding ----
rand_round <- function(x, n) {
  y <- floor(x)
  y + (runif(length(x) * n) < x - y)
}

#---- Largest-remainder rounding ----
largest_remainder_round <- function(p, n) {
  np <- n * p
  npf <- floor(np)
  npf + (rank(npf - np, ties.method = "first") <= n - sum(npf))
}
