#---- Internal helpers ----
# Argument checking
check_inclusion_prob <- function(x, n, s) {
  # sampling::inclusionprobabilities() gives a warning with 0s; I think
  # an error makes more sense
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
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 's' must be the same length")
    )
  }
  if (length(n) != nlevels(s)) {
    stop(
      gettext("'n' must have a single sample size for each level in 's'")
    )
  }
  if (anyNA(s)) {
    stop(
      gettext("'s' cannot contain NAs")
    )
  }
  # bins isn't wide enough by default to catch factors with unused levels
  # at the end
  if (any(tabulate(s, nbins = nlevels(s)) < n)) {
    stop(
      gettext("sample size 'n' is greater than population size")
    )
  }
}

pi <- function(x, n) {
  x * (n / sum(x))
}

.inclusion_prob <- function(x, n) {
  res <- pi(x, n)
  if (length(res) == 0L || max(res) <= 1) return(res)
  repeat {
    keep_ts <- which(res < 1)
    n_ts <- n - length(x) + length(keep_ts)
    res[keep_ts] <- ts <- pi(x[keep_ts], n_ts)
    if (max(ts) <= 1) break
  }
  pmin.int(res, 1)
}

#---- Inclusion probability ----
inclusion_prob <- function(x, n, s = gl(1, length(x))) {
  s <- as.factor(s)
  n <- trunc(n)
  check_inclusion_prob(x, n, s)
  # the single stratum case is common enough to warrant the optimization
  if (nlevels(s) == 1L) {
    .inclusion_prob(x, n)
  } else {
    split(x, s) <- Map(.inclusion_prob, split(x, s), n)
    x
  }
}
