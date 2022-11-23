#---- Internal helpers ----
# Argument checking
check_allocation <- function(x, N, s) {
  # this is stricter than it needs to be, but is consistent with the rest
  # of the API
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
  # needed for tabulate()
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 'strata' must be the same length")
    )
  }
  # missing strata means allocation and coverage are missing
  if (anyNA(s)) {
    stop(
      gettext("'strata' cannot contain NAs")
    )
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

coverage_prob <- function(x, N, s) {
  p <- split(log(1 - .inclusion_prob(x, N)), s)
  1 - vapply(p, function(x) exp(sum(x)), numeric(1L))
}

#---- Expected coverage ----
expected_coverage <- function(x, N, strata = gl(1, length(x))) {
  N <- trunc(N)
  strata <- as.factor(strata)
  check_allocation(x, N, strata)
  sum(coverage_prob(x, N, strata))
}

#---- Proportional allocation ----
prop_allocation <- function(
    x, N, 
    strata = gl(1, length(x)), 
    initial = 0, 
    divisor = function(a) a + 1
) {
  N <- trunc(N)
  strata <- as.factor(strata)
  check_allocation(x, N, strata)
  initial <- trunc(initial)
  if (not_positive_vector(initial)) {
    stop(
      gettext("'initial' must be a positive and finite numeric vector")
    )
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
  res <- highest_averages(divisor)(p, N, initial, ns)
  names(res) <- levels(strata)
  res
}
