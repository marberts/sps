#---- Internal helpers ----
unbounded_pi <- function(x, n) {
  # n == 0 should be a strong zero
  if (n == 0L) {
    rep.int(0, length(x))
  } else {
    x * (n / sum(x))
  }
}

bounded_pi <- function(x, n, alpha) {
  # partial sorting is not stable, so if x[n] == x[n + 1] after sorting then
  # it is possible for the result to not resolve ties according to x
  # (as documented) when alpha is large enough to make at least one unit with
  # x[n] TA
  part <- partition_index(x, n, decreasing = TRUE)
  s <- seq_len(n)
  possible_ta <- sort.int(part[s], decreasing = TRUE)
  definite_ts <- part[seq.int(n + 1, length.out = length(x) - n)]

  # order possible TA units according to x to make subsetting easier,
  # noting that ties will be in reverse
  possible_ta <- possible_ta[order(x[possible_ta])]
  y <- x[possible_ta]

  # the sequence given by p has the following properties
  # 1. if p[k] < 1, then p[k + 1] >= p[k]
  # 2. if p[k] >= 1, then p[k + 1] >= 1
  # consequently, if p[k] >= 1 - alpha, then p[k + m] >= 1 - alpha
  p <- y * s / (sum(x[definite_ts]) + cumsum(y))
  ta <- possible_ta[p >= 1 - alpha]
  ts <- c(definite_ts, setdiff(possible_ta, ta))

  res <- rep_len(1, length(x))
  res[ts] <- unbounded_pi(x[ts], n - length(ta))
  res
}

pi <- function(x, n, alpha) {
  if (any(x < 0)) {
    stop("'x' must be greater than or equal to 0")
  }
  if (n < 0L) {
    stop("'n' must be greater than or equal to 0")
  }
  if (n > sum(x > 0)) {
    stop(
      "sample size is greater than the number of units with ",
      "non-zero sizes in the population"
    )
  }
  if (alpha < 0 || alpha >= 1) {
    stop("'alpha' must be in [0, 1)")
  }

  res <- unbounded_pi(x, n)
  if (all(res < 1 - alpha)) {
    res
  } else {
    bounded_pi(x, n, alpha)
  }
}

inclusion_prob_ <- function(x, n, strata, alpha) {
  if (length(x) != length(strata)) {
    stop("'x' and 'strata' must be the same length")
  }
  if (anyNA(strata)) {
    stop("'strata' cannot contain NAs")
  }
  if (nlevels(strata) < 1L) {
    stop("'strata' must have one or more levels")
  }
  if (length(n) != nlevels(strata)) {
    stop("'n' must have a single sample size for each level in 'strata'")
  }
  if (length(alpha) != 1L && length(alpha) != nlevels(strata)) {
    stop(
      "'alpha' must have a single value or a value for each level in 'strata'"
    )
  }
  Map(pi, split(x, strata), n, alpha)
}

#---- Inclusion probability ----
inclusion_prob <- function(x, n, strata = gl(1, length(x)), alpha = 1e-4) {
  x <- as.numeric(x)
  n <- as.integer(n)
  strata <- as.factor(strata)
  alpha <- as.numeric(alpha)
  unsplit(inclusion_prob_(x, n, strata, alpha), strata)
}
