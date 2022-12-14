#---- Internal helpers ----
# Argument checking
check_allocation <- function(x, N, strata) {
  if (min(x) < 0) {
    stop(gettext("'x' must be positive"))
  }
  if (N < 0) {
    stop(gettext("'N' must be positive"))
  }
  if (N > length(x)) {
    stop(gettext("sample size 'N' is greater than population size"))
  }
  # needed for tabulate()
  if (length(x) != length(strata)) {
    stop(gettext("'x' and 'strata' must be the same length"))
  }
  # missing strata means allocation and coverage are missing
  if (anyNA(strata)) {
    stop(gettext("'strata' cannot contain NAs"))
  }
}

# Apportionment (rounding) method
highest_averages <- function(d) {
  d <- match.fun(d)
  # return function
  function(p, n, min, max) {
    res <- as.vector(min) # strip attributes
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

coverage_prob <- function(x, N, s, alpha) {
  p <- split(log(1 - .inclusion_prob(x, N, alpha)), s)
  1 - vapply(p, function(x) exp(sum(x)), numeric(1L))
}

#---- Expected coverage ----
expected_coverage <- function(
    x, N, 
    strata = gl(1, length(x)), 
    alpha = 0
) {
  x <- as.numeric(x)
  N <- trunc(as.numeric(N))
  strata <- as.factor(strata)
  alpha <- as.numeric(alpha)
  check_allocation(x, N, strata)
  if (alpha < 0 || alpha >= 1) {
    stop(gettext("'alpha' must be in [0, 1)"))
  }
  sum(coverage_prob(x, N, strata, alpha))
}

#---- Proportional allocation ----
prop_allocation <- function(
    x, N, 
    strata = gl(1, length(x)), 
    initial = 0, 
    divisor = function(a) a + 1
) {
  x <- as.numeric(x)
  N <- trunc(as.numeric(N))
  strata <- as.factor(strata)
  initial <- trunc(as.numeric(initial))
  check_allocation(x, N, strata)
  if (min(initial) < 0) {
    stop(gettext("'initial' must be positive"))
  }
  ns <- tabulate(strata, nbins = nlevels(strata))
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
      gettext("'initial' must be smaller than the population size for each stratum")
    )
  }
  if (N < sum(initial)) {
    stop(
      gettext("initial allocation is larger than 'N'")
    )
  }
  p <- vapply(split(x, strata), sum, numeric(1L))
  if (!any(p > 0)) {
    stop(gettext("all strata have zero size"))
  }
  res <- highest_averages(divisor)(p, N, initial, ns)
  names(res) <- levels(strata)
  res
}
