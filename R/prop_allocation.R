#---- Internal functions ----
# Apportionment (rounding) method
highest_averages <- function(d) {
  d <- match.fun(d)

  function(p, n, initial, available, ties = c("largest", "first")) {
    if (n < 0L) {
      stop("sample size must be greater than or equal to 0")
    }
    if (length(p) != length(initial)) {
      stop("initial allocation must have a single size for each stratum")
    }
    if (any(initial < 0)) {
      stop("initial allocation must be greater than or equal to 0")
    }
    if (n < sum(initial)) {
      stop("initial allocation cannot be larger than sample size")
    }
    if (length(available) != length(initial)) {
      stop("number of available units in the population must be specified for ",
           "each stratum")
    }
    if (n > sum(available)) {
      stop(
        "sample size cannot be greater than the number of available units in ",
        "the population"
      )
    }
    if (any(initial > available)) {
      stop(
        "initial allocation must be smaller than the number of available ",
        "units in the population for each stratum"
      )
    }

    res <- initial
    n <- n - sum(res)
    ord <- switch(
      match.arg(ties),
      largest = order(p, decreasing = TRUE),
      first = seq_along(p)
    )
    p <- p[ord]
    res <- res[ord]
    available <- available[ord]
    # the while condition could be n > sum(res), but the loop below always
    # terminates after at most n steps, even if i is integer(0)
    while (n > 0L) {
      i <- which.max(p / d(res) * (res < available))
      res[i] <- res[i] + 1L
      n <- n - 1L
    }
    res[order(ord)]
  }
}

#---- Expected coverage ----
expected_coverage <- function(x, n, strata, alpha = 1e-3, cutoff = Inf) {
  x <- as.numeric(x)
  n <- as.integer(n)
  strata <- as_stratum(strata)
  alpha <- as.numeric(alpha)
  cutoff <- as.numeric(cutoff)
  if (length(x) != length(strata)) {
    stop("the vectors for sizes and strata must be the same length")
  }
  p <- split(log(1 - inclusion_prob(x, n, alpha = alpha, cutoff = cutoff)),
             strata)
  sum(1 - vapply(p, function(x) exp(sum(x)), numeric(1L)))
}

#---- Proportional allocation ----
prop_allocation <- function(
  x, n, strata, initial = 0L, divisor = \(a) a + 1, ties = c("largest", "first")
) {
  x <- as.numeric(x)
  n <- as.integer(n)
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
    initial <- pmin.int(ns, min(n %/% nlevels(strata), initial))
  }

  p <- vapply(x, sum, numeric(1L))
  res <- highest_averages(divisor)(p, n, initial, ns, ties)
  names(res) <- levels(strata)
  res
}
