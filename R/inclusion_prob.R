#---- Internal helpers ----
check_inclusion_prob <- function(x, n, strata, alpha) {
  if (min(x) < 0) {
    stop(gettext("'x' must be positive"))
  }
  if (min(n) < 0) {
    stop(gettext("'n' must be positive"))
  }
  if (length(x) != length(strata)) {
    stop(gettext("'x' and 'strata' must be the same length"))
  }
  if (length(n) != nlevels(strata)) {
    stop(
      gettext("'n' must have a single sample size for each level in 'strata'")
    )
  }
  # missing strata means inclusion probs are all missing
  if (anyNA(strata)) {
    stop(gettext("'strata' cannot contain NAs"))
  }
  if (alpha < 0 || alpha >= 1) {
    stop(gettext("'alpha' must be in [0, 1)"))
  }
}

pi <- function(x, n) {
  x * (n / sum(x))
}

.inclusion_prob <- function(x, n, alpha = 0) {
  if (n > sum(x > 0)) {
    stop(
      gettext("sample size is greater than the number of units with non-zero sizes in the population")
    )
  }
  res <- as.numeric(pi(x, n)) # strip attributes
  if (length(res) > 0L && max(res) > 1) {
    repeat {
      # inclusion probs increase with each loop, so only need to 
      # recalculate those strictly less than 1
      keep_ts <- which(res < 1)
      n_ts <- n - length(x) + length(keep_ts)
      res[keep_ts] <- ts <- pi(x[keep_ts], n_ts)
      if (max(ts) <= 1) break
    }
    res <- pmin.int(res, 1)
  }
  if (alpha > 0) {
    res[res >= 1 - alpha] <- 1
    if (sum(res == 1) > n) {
      stop(gettext("'alpha' is too large given 'n'"))
    }
  }
  res
}

#---- Inclusion probability ----
inclusion_prob <- function(x, n, strata = gl(1, length(x)), alpha = 0) {
  x <- as.numeric(x)
  n <- trunc(as.numeric(n))
  strata <- as.factor(strata)
  alpha <- as.numeric(alpha)
  check_inclusion_prob(x, n, strata, alpha)
  # the single stratum case is common enough to warrant the optimization
  if (nlevels(strata) == 1L) {
    .inclusion_prob(x, n, alpha)
  } else {
    unsplit(Map(.inclusion_prob, split(x, strata), n, alpha), strata)
  }
}
