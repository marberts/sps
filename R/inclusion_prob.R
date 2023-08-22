#---- Internal functions ----
as_stratum <- function(strata) {
  strata <- as.factor(strata)
  if (anyNA(strata)) {
    stop("cannot have missing strata")
  }
  if (nlevels(strata) < 1L) {
    stop("there must be at least one stratum")
  }
  strata
}

unbounded_pi <- function(x, n) {
  # n == 0 should be a strong zero
  if (n == 0L) {
    rep.int(0, length(x))
  } else {
    x * (n / sum(x))
  }
}

ta_units <- function(x, n, alpha) {
  # partial sorting is not stable, so if x[n] == x[n + 1] after sorting then
  # it is possible for the result to not resolve ties according to x
  # (as documented) when alpha is large enough to make at least one unit with
  # x[n] TA
  ord <- order(x, decreasing = TRUE)
  s <- seq_len(n)
  possible_ta <- rev(ord[s])
  x_ta <- x[possible_ta] # ties are in reverse
  definite_ts <- ord[seq.int(n + 1, length.out = length(x) - n)]

  p <- x_ta * s / (sum(x[definite_ts]) + cumsum(x_ta))
  # the sequence given by p has the following properties
  # 1. if p[k] < 1, then p[k + 1] >= p[k]
  # 2. if p[k] >= 1, then p[k + 1] >= 1
  # consequently, if p[k] >= 1 - alpha, then p[k + m] >= 1 - alpha
  possible_ta[p >= 1 - alpha]
}

pi <- function(x, n, alpha, cutoff) {
  if (any(x < 0)) {
    stop("sizes must be greater than or equal to 0")
  }
  if (n < 0L) {
    stop("sample size must be greater than or equal to 0")
  }
  if (n > sum(x > 0)) {
    stop(
      "sample size cannot be greater than the number of available units with ",
      "non-zero sizes in the population"
    )
  }
  if (alpha < 0 || alpha > 1) {
    stop("'alpha' must be between 0 and 1")
  }
  if (cutoff <= 0) {
    stop("'cutoff' must be greater than 0")
  }

  ta <- which(x >= cutoff)
  if (length(ta) > n) {
    stop("'n' is not large enough to include all units with 'x' above 'cutoff'")
  }

  x[ta] <- 0
  res <- unbounded_pi(x, n - length(ta))
  if (any(res >= 1 - alpha)) {
    ta2 <- ta_units(x, n - length(ta), alpha)
    x[ta2] <- 0
    ta <- c(ta, ta2)
    res <- unbounded_pi(x, n - length(ta))
  }

  res[ta] <- 1
  res
}

inclusion_prob_ <- function(x, n, strata, alpha, cutoff) {
  if (length(x) != length(strata)) {
    stop("the vectors for sizes and strata must be the same length")
  }
  if (length(n) != 1L && length(n) != nlevels(strata)) {
    stop(
      "there must be a single sample size",
      if (nlevels(strata) > 1) " for each stratum"
    )
  }
  if (length(alpha) != 1L && length(alpha) != nlevels(strata)) {
    stop(
      "'alpha' must be a single value",
      if (nlevels(strata) > 1) " or have a value for each stratum"
    )
  }
  if (length(cutoff) != 1L && length(cutoff) != nlevels(strata)) {
    stop(
      "'cutoff' must be a single value",
      if (nlevels(strata) > 1) " or have a value for each stratum"
    )
  }
  Map(pi, split(x, strata), n, alpha, cutoff)
}

#---- Inclusion probability ----
inclusion_prob <- function(x,
                           n,
                           strata = gl(1, length(x)),
                           alpha = 1e-3,
                           cutoff = Inf) {
  x <- as.numeric(x)
  n <- as.integer(n)
  strata <- as_stratum(strata)
  alpha <- as.numeric(alpha)
  cutoff <- as.numeric(cutoff)
  unsplit(inclusion_prob_(x, n, strata, alpha, cutoff), strata)
}
