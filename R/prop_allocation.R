#---- Internal functions ----
# Apportionment (rounding) method
highest_averages <- function(d) {
  d <- match.fun(d)

  function(p, N, initial, upper, ties = c("largest", "first")) {
    if (N < 0L) {
      stop("sample size must be greater than or equal to 0")
    }
    if (length(p) != length(initial)) {
      stop("initial allocation must have a single size for each stratum")
    }
    if (any(initial < 0)) {
      stop("initial allocation must be greater than or equal to 0")
    }
    if (N < sum(initial)) {
      stop("initial allocation cannot be larger than sample size")
    }
    if (N > sum(upper)) {
      stop(
        "sample size cannot be greater than the number of units with non-zero ",
        "sizes in the population"
      )
    }
    if (any(initial > upper)) {
      stop(
        "initial allocation must be smaller than the number of units with ",
        "non-zero sizes in the population for each stratum"
      )
    }

    res <- initial
    N <- N - sum(res)
    ord <- switch(
      match.arg(ties),
      largest = order(p, decreasing = TRUE),
      first = seq_along(p)
    )
    p <- p[ord]
    res <- res[ord]
    upper <- upper[ord]
    # the while condition could be n > sum(res), but the loop below always
    # terminates after at most n steps, even if i is integer(0)
    while (N > 0L) {
      i <- which.max(p / d(res) * (res < upper))
      res[i] <- res[i] + 1L
      N <- N - 1L
    }
    res[order(ord)]
  }
}

#---- Expected coverage ----
expected_coverage <- function(x, N, strata, alpha = 1e-3) {
  x <- as.numeric(x)
  N <- as.integer(N)
  alpha <- as.numeric(alpha)
  strata <- as_stratum(strata)
  if (length(x) != length(strata)) {
    stop("the vectors for sizes and strata must be the same length")
  }
  p <- split(log(1 - inclusion_prob(x, N, alpha = alpha)), strata)
  sum(1 - vapply(p, function(x) exp(sum(x)), numeric(1L)))
}

#---- Proportional allocation ----
prop_allocation <- function(
  x, N, strata, initial = 0, divisor = \(a) a + 1, ties = c("largest", "first")
) {
  x <- as.numeric(x)
  N <- as.integer(N)
  strata <- as_stratum(strata)
  initial <- as.integer(initial)

  if (any(x < 0)) {
    stop("sizes must be greater than or equal to 0")
  }
  if (length(x) != length(strata)) {
    stop("the vectors for sizes and strata must be the same length")
  }

  x <- split(x, strata)
  ns <- vapply(x, function(x) sum(x > 0), integer(1L))

  if (length(initial) == 1L) {
    initial <- pmin.int(ns, min(N %/% nlevels(strata), initial))
  }

  p <- vapply(x, sum, numeric(1L))
  res <- highest_averages(divisor)(p, N, initial, ns, ties)
  names(res) <- levels(strata)
  res
}
