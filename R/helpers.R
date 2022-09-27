# None of these functions are exported
#---- Argument checking ----
not_positive_vector <- function(x) {
  !(is.numeric(x) && all(is.finite(x)) && all(x >= 0))
}

not_strict_positive_vector <- function(x) {
  !(is.numeric(x) && all(is.finite(x)) && all(x > 0))
}

not_positive_number <- function(x) {
  length(x) != 1L || not_positive_vector(x)
}

not_prob <- function(x) {
  not_strict_positive_vector(x) || any(x >= 1)
}

#---- Inclusion probability ----
inclusion_prob <- function(x, n) {
  x / sum(x) * n
}

#---- Random rounding ----
random_round <- function(x, n) {
  y <- floor(x)
  y + (runif(length(x) * n) < x - y)
}

#---- Rounding methods ----
largest_remainder <- function(p, n, initial) {
  p <- p / sum(p)
  np <- n * p
  npf <- floor(np)
  remainder <- rank(npf - np, ties.method = "first") <= n - sum(npf)
  initial + npf + remainder
}

highest_averages <- function(divisor) {
  f <- switch(
    divisor,
    "D'Hondt" = function(x) x + 1,
    "Webster" = function(x) x + 0.5,
    "Imperiali" = function(x) x + 2,
    "Huntington-Hill" = function(x) sqrt(x * (x + 1)),
    "Danish" = function(x) x + 1 / 3,
    "Adams" = function(x) x,
    "Dean" = function(x) x * (x + 1) / (x + 0.5)
  )
  # return function
  res <- function(p, n, initial) {
    if (is.null(names(initial))) {
      names(initial) <- names(p)
    }
    while (n > 0) {
      i <- which.max(p / f(initial))
      initial[i] <- initial[i] + 1
      n <- n - 1
    }
    initial
  }
  environment(res) <- list2env(list(f = f), parent = getNamespace("sps"))
  res
}
