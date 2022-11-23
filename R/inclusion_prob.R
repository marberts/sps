#---- Internal helpers ----
# Argument checking
check_inclusion_prob <- function(x, n, s) {
  # sampling::inclusionprobabilities() gives a warning with 0s; I think
  # an error makes more sense
  # e.g., x = c(0, 0, 1, 1) and n = 3 => units 1 and 2 have inclusion probs
  # of 0, but at least one must be included in the sample
  if (not_strict_positive_vector(x)) {
    stop(
      gettext("'x' must be a strictly positive and finite numeric vector")
    )
  }
  if (not_positive_vector(n)) {
    stop(
      gettext("'n' must be a positive and finite numeric vector")
    )
  }
  # needs to be the same length for tabulate()
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 'strata' must be the same length")
    )
  }
  if (length(n) != nlevels(s)) {
    stop(
      gettext("'n' must have a single sample size for each level in 'strata'")
    )
  }
  # missing strata means inclusion probs are all missing
  if (anyNA(s)) {
    stop(
      gettext("'strata' cannot contain NAs")
    )
  }
  # bins isn't wide enough by default to catch factors with unused levels
  # at the end
  if (any(tabulate(s, nbins = nlevels(s)) < n)) {
    stop(
      gettext("sample size 'n' is greater than population size for some strata")
    )
  }
}

pi <- function(x, n) {
  x * (n / sum(x))
}

.inclusion_prob <- function(x, n) {
  res <- pi(x, n)
  if (length(res) == 0L || max(res) <= 1) return(as.numeric(res))
  repeat {
    # inclusion probs increase with each loop, so only need to recalculate
    # those strictly less than 1
    keep_ts <- which(res < 1)
    n_ts <- n - length(x) + length(keep_ts)
    res[keep_ts] <- ts <- pi(x[keep_ts], n_ts)
    if (max(ts) <= 1) break
  }
  pmin.int(res, 1)
}

#---- Inclusion probability ----
inclusion_prob <- function(x, n, strata = gl(1, length(x))) {
  strata <- as.factor(strata)
  n <- trunc(n)
  check_inclusion_prob(x, n, strata)
  # the single stratum case is common enough to warrant the optimization
  if (nlevels(strata) == 1L) {
    .inclusion_prob(x, n)
  } else {
    unsplit(Map(.inclusion_prob, split(x, strata), n), strata)
  }
}
