#---- Sequential Poisson sampling (internal) ----
.sps <- function(x, n) {
  x <- as.numeric(x)
  if (!is_positive_numeric(x)) stop("'x' must be > 0 and finite")
  if (!is_positive_number(n)) stop("'n' must be a number > 0")
  N <- length(x)
  if (n >= N) {
    warning("sample size is greater than or equal to population size")
    n <- min(n, N)
  }
  # remove the take alls so that inclusion probs are < 1
  stratum <- replace(rep("ts", N), incl_prob(x, n) >= 1, "ta")
  res <- split(seq_len(N), stratum)
  n_ts <- n - length(res$ta) # always >= 0
  ts_to_ta <- (p <- incl_prob(x[res$ts], n_ts)) >= 1
  while (any(ts_to_ta, na.rm = TRUE)) { # repeat until all inclusion probs are < 1
    res$ta <- c(res$ta, res$ts[ts_to_ta])
    res$ts <- res$ts[!ts_to_ta]
    n_ts <- n - length(res$ta)
    ts_to_ta <- (p <- incl_prob(x[res$ts], n_ts)) >= 1
  }
  # sample the take somes
  z <- runif(length(res$ts)) / p
  keep <- order(z)[seq_len(n_ts)]
  res$ts <- res$ts[keep]
  structure(unlist(res, use.names = FALSE),
            weights = c(rep(1, n - n_ts), 1 / p[keep]),
            levels = rep(c("TA", "TS"), c(n - n_ts, n_ts)),
            class = c("sample", "numeric"))
}

#---- Make a stratified sampling method ----
stratify <- function(fun) {
  fun <- match.fun(fun)
  function(x, n, s = rep(1, length(x))) {
    if (length(x) != length(s)) stop("'x' and 's' must be the same length")
    s <- as.factor(s)
    if (length(n) != nlevels(s)) {
      stop("'n' must have a single sample size for each level in 's' (stratum)")
    }
    samp <- Map(fun, split(x, s), n)
    res <- Map(`[`, split(seq_along(x), s), samp)
    structure(unlist(res, use.names = FALSE),
              weights = unlist(lapply(samp, weights), use.names = FALSE),
              levels = unlist(lapply(samp, levels), use.names = FALSE),
              class = c("sample", "numeric"))
  }
}

#---- Sequential Poisson sampling (external) ----
sps <- stratify(.sps)

bs_weights <- function(x, n = 1000, dist = rnorm, tau = 1) {
  if (!is_positive_number(tau) || tau < 1) stop("'tau' must be a number >= 1")
  if (n < 100 || n > 10000) warning("'n' should be between 100 and 10,000")
  a <- matrix(dist(length(x) * n), ncol = n)
  res <- (x * a * sqrt(1 - 1 / x) + tau) / tau
  if (any(res < 0)) warning("Negative replicate weights; try increasing 'tau'")
  res
}