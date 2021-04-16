#---- Sequential Poisson sampling (internal) ----
.sps <- function(x, n) {
  N <- length(x)
  if (n >= N) {
    warning("sample size 'n' is greater than or equal to population size")
    n <- min(n, N)
  }
  # remove the take alls so that inclusion probs are < 1
  stratum <- replace(rep("ts", N), inclusion_prob(x, n) >= 1, "ta")
  res <- split(seq_len(N), stratum)
  repeat { # repeat until all inclusion probs are < 1
    n_ts <- n - length(res$ta) # always >= 0
    ts_to_ta <- (p <- inclusion_prob(x[res$ts], n_ts)) >= 1
    res$ta <- c(res$ta, res$ts[ts_to_ta])
    res$ts <- res$ts[!ts_to_ta]
    if (!any(ts_to_ta, na.rm = TRUE)) break
  }
  # sample the take somes
  z <- runif(length(res$ts)) / p
  keep <- order(z)[seq_len(n_ts)]
  res$ts <- res$ts[keep]
  structure(unlist(res, use.names = FALSE),
            weights = c(rep(1, n - n_ts), 1 / p[keep]),
            levels = rep(c("TA", "TS"), c(n - n_ts, n_ts)),
            class = c("sps", "numeric"))
}

#---- Stratified sequential Poisson sampling (exported)----
sps <- function(x, n, s = rep(1L, length(x))) {
  if (!is_positive_numeric(x)) {
    stop("'x' must be a positive and finite numeric vector")
  }
  if (!is_positive_numeric1(n)) {
    stop("'n' must be a finite numeric vector with each element greater than or equal to 1")
  }
  s <- as.factor(s)
  if (length(x) != length(s)) {
    stop("'x' and 's' must be the same length")
  }
  if (length(n) != nlevels(s)) {
    stop("'n' must have a single sample size for each level in 's' (stratum)")
  }
  samp <- Map(.sps, split(x, s), trunc(n))
  res <- Map(`[`, split(seq_along(x), s), samp)
  structure(unlist(res, use.names = FALSE),
            weights = unlist(lapply(samp, weights), use.names = FALSE),
            levels = unlist(lapply(samp, levels), use.names = FALSE),
            class = c("sps", "numeric"))
}

#---- Methods for class 'sps' ----
# levels.default() extracts levels; no need for a method

weights.sps <- function(object, ...) {
  attr(object, "weights")
}

print.sps <- function(x, ...) {
  print(as.numeric(x), ...)
  invisible(x)
}
