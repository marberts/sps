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
    while (n > 0) {
      i <- which.max(p / d(res) * (res < max))
      res[i] <- res[i] + 1L
      n <- n - 1L
    }
    res
  }
}

#---- Expected coverage ----
expected_coverage <- function(
    x, N, 
    strata = gl(1, length(x)), 
    alpha = 0
) {
  x <- as.numeric(x)
  if (min(x) < 0) {
    stop(gettext("'x' must be positive"))
  }
  
  N <- trunc(as.numeric(N))
  if (N < 0) {
    stop(gettext("'N' must be positive"))
  }
  
  strata <- as.factor(strata)
  if (length(x) != length(strata)) {
    stop(gettext("'x' and 'strata' must be the same length"))
  }
  if (anyNA(strata)) {
    stop(gettext("'strata' cannot contain NAs"))
  }
  
  alpha <- as.numeric(alpha)
  if (alpha < 0 || alpha >= 1) {
    stop(gettext("'alpha' must be in [0, 1)"))
  }
  
  p <- split(log(1 - .inclusion_prob(x, N, alpha)), strata)
  sum(1 - vapply(p, function(x) exp(sum(x)), numeric(1L)))
}

#---- Proportional allocation ----
prop_allocation <- function(
    x, N, 
    strata = gl(1, length(x)), 
    initial = 0, 
    divisor = function(a) a + 1
) {
  x <- as.numeric(x)
  if (min(x) < 0) {
    stop(gettext("'x' must be positive"))
  }
  
  N <- trunc(as.numeric(N))
  if (N < 0) {
    stop(gettext("'N' must be positive"))
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
  # missing strata means allocation and coverage are missing
  if (anyNA(strata)) {
    stop(gettext("'strata' cannot contain NAs"))
  }
  x <- split(x, strata)
  ns <- vapply(x, function(x) sum(x > 0), numeric(1L))
  
  initial <- trunc(as.numeric(initial))
  if (min(initial) < 0) {
    stop(gettext("'initial' must be positive"))
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
