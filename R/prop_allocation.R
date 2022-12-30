#---- Internal helpers ----
# Apportionment (rounding) method
highest_averages <- function(d) {
  d <- match.fun(d)
  # return function
  function(p, n, min, max) {
    res <- min
    n <- n - sum(res)
    # the while condition could be n > sum(res), but the loop below always
    # terminates after at most n steps, even if i is integer(0)
    while (n > 0L) {
      i <- which.max(p / d(res) * (res < max))
      res[i] <- res[i] + 1L
      n <- n - 1L
    }
    res
  }
}

#---- Expected coverage ----
expected_coverage <- function(x, N, strata, alpha = 1e-4) {
  x <- as.numeric(x)
  if (.min(x) < 0) {
    stop(gettext("'x' must be greater than or equal to 0"))
  }
  
  N <- as.integer(N)
  if (N < 0L) {
    stop(gettext("'N' must be greater than or equal to 0"))
  }
  
  strata <- as.factor(strata)
  if (length(x) != length(strata)) {
    stop(gettext("'x' and 'strata' must be the same length"))
  }
  if (anyNA(strata)) {
    stop(gettext("'strata' cannot contain NAs"))
  }
  
  alpha <- as.numeric(alpha)
  if (alpha >= 1 || alpha < 0) {
    stop(gettext("'alpha' must be in [0, 1)"))
  }
  
  p <- split(log(1 - .inclusion_prob(x, N, alpha)), strata)
  sum(1 - vapply(p, function(x) exp(sum(x)), numeric(1L)))
}

#---- Proportional allocation ----
prop_allocation <- function(
    x, N, strata,
    initial = 0, 
    divisor = function(a) a + 1
) {
  x <- as.numeric(x)
  if (.min(x) < 0) {
    stop(gettext("'x' must be greater than or equal to 0"))
  }
  
  N <- as.integer(N)
  if (N < 0L) {
    stop(gettext("'N' must be greater than or equal to 0"))
  }
  if (N > sum(x > 0)) {
    stop(
      gettext("sample size is greater than the number of units with non-zero sizes in the population")
    )
  }
  
  strata <- as.factor(strata)
  if (length(x) != length(strata)) {
    stop(gettext("'x' and 'strata' must be the same length"))
  }
  if (nlevels(strata) < 1L) {
    stop(gettext("cannot allocate to no strata"))
  }
  # missing strata means allocation and coverage are missing
  if (anyNA(strata)) {
    stop(gettext("'strata' cannot contain NAs"))
  }
  x <- split(x, strata)
  ns <- vapply(x, function(x) sum(x > 0), integer(1L))
  
  initial <- as.integer(initial)
  if (min(initial) < 0L) {
    stop(gettext("'initial' must be greater than or equal to 0"))
  }
  if (length(initial) == 1L) {
    initial <- pmin.int(ns, min(N %/% nlevels(strata), initial))
  }
  if (length(initial) != nlevels(strata)) {
    stop(
      gettext("'initial' must have a single allocation size for each level in 'strata'")
    )
  }
  if (any(initial > ns)) {
    stop(
      gettext("'initial' must be smaller than the number of units with non-zero sizes in the population for each stratum")
    )
  }
  if (N < sum(initial)) {
    stop(gettext("initial allocation is larger than 'N'"))
  }
  
  p <- vapply(x, sum, numeric(1L))
  res <- highest_averages(divisor)(p, N, initial, ns)
  names(res) <- levels(strata)
  res
}
