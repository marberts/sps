#---- Internal helpers ----
# Apportionment (rounding) methods
largest_remainder <- function(p, n, initial) {
  p <- p * (n / sum(p))
  np <- floor(p)
  res <- initial + np
  remainder <- order(np - p)[seq_len(n - sum(np))]
  res[remainder] <- res[remainder] + 1
  res
}

highest_averages <- function(divisor) {
  divisor <- switch(
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
  function(p, n, initial) {
    while (n > 0) {
      i <- which.max(p / divisor(initial))
      initial[i] <- initial[i] + 1
      n <- n - 1
    }
    initial
  }
}

# Argument checking
check_allocation <- function(x, N, s) {
  if (not_strict_positive_vector(x)) {
    stop(
      gettext("'x' must be a strictly positive and finite numeric vector")
    )
  }
  if (not_positive_number(N)) {
    stop(
      gettext("'N' must be a positive and finite number")
    )
  }
  if (N > length(x)) {
    stop(
      gettext("sample size 'N' is greater than population size")
    )
  }
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 's' must be the same length")
    )
  }
  if (anyNA(s)) {
    stop(
      gettext("'s' cannot contain NAs")
    )
  }
}

#---- Expected coverage ----
expected_coverage <- function(x, N, s = gl(1, length(x))) {
  N <- trunc(N)
  s <- as.factor(s)
  check_allocation(x, N, s)
  p <- split(log(1 - .inclusion_prob(x, N)), s)
  sum(1 - vapply(p, function(x) exp(sum(x)), numeric(1L)))
}

#---- Proportional allocation ----
prop_allocation <- function(
    x, N, s = gl(1, length(x)), initial = 0, 
    method = c("Largest-remainder", "D'Hondt", "Webster", "Imperiali", 
               "Huntington-Hill", "Danish", "Adams", "Dean")  
) {
  N <- trunc(N)
  s <- as.factor(s)
  check_allocation(x, N, s)
  initial <- trunc(initial)
  if (not_positive_vector(initial)) {
    stop(
      gettext("'initial' must be a positive and finite numeric vector")
    )
  }
  ns <- tabulate(s, nbins = nlevels(s))
  if (length(initial) == 1L) {
    initial <- pmin(ns, min(N %/% nlevels(s), initial))
  }
  if (length(initial) != nlevels(s)) {
    stop(
      gettext("'initial' must have a single allocation size for each level in 's'")
    )
  }
  if (any(initial > ns)) {
    stop(
      gettext("'initial' must be smaller than the population size for each stratum")
    )
  }
  N <- N - sum(initial)
  if (N < 0) {
    stop(
      gettext("initial allocation is larger than 'N'")
    )
  }
  method <- match.arg(method)
  apportionment <- if (method == "Largest-remainder") {
    largest_remainder
  } else {
    highest_averages(method)
  }
  res <- initial
  p <- vapply(split(x, s), sum, numeric(1L))
  repeat {
    res <- apportionment(p, N, res)
    d <- pmax(res - ns, 0)
    over <- which(as.logical(d))
    if (length(over) == 0L) break
    # redistribute sample units for those that cap out at the stratum size
    res[over] <- ns[over]
    p[over] <- 0  
    N <- sum(d)
  }
  structure(res, names = levels(s))
}
