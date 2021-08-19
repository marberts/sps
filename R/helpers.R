# None of these functions are exported
#---- Argument checking ----
not_positive_vector <- function(x) {
  !(is.numeric(x) && all(is.finite(x)) && all(x >= 0))
}

not_strict_positive_vector <- function(x) {
  !(is.numeric(x) && all(is.finite(x)) && all(x > 0))
}

not_positive_number <- function(x) {
  length(x) != 1 || not_positive_vector(x)
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
