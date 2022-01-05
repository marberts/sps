#---- Sequential Poisson sampling (internal) ----
.sps <- function(x, n, u = runif(length(x))) {
  N <- length(x)
  if (n > N) {
    stop(gettext("sample size 'n' is greater than or equal to population size"))
  }
  # remove the take alls so that inclusion probs are < 1
  stratum <- replace(rep("ts", N), inclusion_prob(x, n) >= 1, "ta")
  res <- split(seq_len(N), factor(stratum, levels = c("ta", "ts")))
  repeat { # repeat until all inclusion probs are < 1
    n_ts <- n - length(res$ta) # always >= 0
    ts_to_ta <- (p <- inclusion_prob(x[res$ts], n_ts)) >= 1
    res$ta <- c(res$ta, res$ts[ts_to_ta])
    res$ts <- res$ts[!ts_to_ta]
    if (!any(ts_to_ta)) break
  }
  # sample the take somes
  keep <- if (n_ts) {
    z <- u[res$ts] / p
    order(z)[seq_len(n_ts)]
  }
  res$ts <- res$ts[keep]
  res <- unlist(res, use.names = FALSE) # unlist can return NULL
  if (!length(res)) res <- integer(0L)
  structure(res,
            weights = c(rep(1, n - n_ts), 1 / p[keep]),
            levels = rep(c("TA", "TS"), c(n - n_ts, n_ts)),
            class = c("sps", class(res)))
}

#---- Stratified sequential Poisson sampling (exported)----
sps <- function(x, n, s = rep(1L, length(x)), u = runif(length(x))) {
  if (not_strict_positive_vector(x)) {
    stop(gettext("'x' must be a strictly positive and finite numeric vector"))
  }
  n <- trunc(n)
  if (not_positive_vector(n)) {
    stop(gettext("'n' must be a positive and finite numeric vector"))
  }
  if (length(x) != length(s)) {
    stop(gettext("'x' and 's' must be the same length"))
  }
  s <- as.factor(s)
  if (length(n) != nlevels(s)) {
    stop(gettext("'n' must have a single sample size for each level in 's' (stratum)"))
  }
  if (length(x) != length(u)) {
    stop(gettext("'x' and 'u' must be the same length"))
  }
  if (not_prob(u)) {
    stop(gettext("u' must be a numeric vector between 0 and 1"))
  }
  samp <- .mapply(.sps, list(split(x, s), n, split(u, s)), list())
  res <- .mapply(`[`, list(split(seq_along(x), s), samp), list())
  res <- unlist(res, use.names = FALSE) # unlist can return NULL
  if (!length(res)) res <- integer(0L)
  structure(res,
            weights = as.numeric(unlist(lapply(samp, weights), use.names = FALSE)),
            levels = as.character(unlist(lapply(samp, levels), use.names = FALSE)),
            class = c("sps", class(res)))
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
