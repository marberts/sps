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
rand_round <- function(x, n) {
  y <- floor(x)
  y + (runif(length(x) * n) < x - y)
}

#---- Largest-remainder rounding ----
largest_remainder_round <- function(p, n, a) {
  p <- p / sum(p)
  np <- n * p
  npf <- floor(np)
  a + npf + (rank(npf - np, ties.method = "first") <= n - sum(npf))
}

highest_averages <- function(
    divisor = c("D'Hondt", "Webster", "Imperiali", 
                "Huntington-Hill", "Danish", "Adams", "Dean")
) {
  f <- switch(
    match.arg(divisor),
    "D'Hondt" = function(x) x + 1,
    "Webster" = function(x) x + 0.5,
    "Imperiali" = function(x) x + 2,
    "Huntington-Hill" = function(x) sqrt(x * (x + 1)),
    "Danish" = function(x) x + 1 / 3,
    "Adams" = function(x) x,
    "Dean" = function(x) x * (x + 1) / (x + 0.5)
  )
  function(p, n, a) {
    if (is.null(names(a))) {
      names(a) <- names(p)
    }
    while (n > 0) {
      i <- which.max(p / f(a))
      a[i] <- a[i] + 1
      n <- n - 1
    }
    a
  }
}
