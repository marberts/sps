#---- Internal sampling functions ----
# Sequential Poisson sampling
.sps <- function(x, n) {
  stopifnot("'x' must be a strictly positive and finite numeric vector" = is_positive_numeric(x))
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

# Simple random sampling
.srs <- function(x, n) {
  N <- length(x)
  if (n >= N) {
    warning("sample size is greater than or equal to population size")
    n <- min(n, N)
  }
  z <- runif(N)
  res <- order(z)[seq_len(n)]
  structure(res,
            weights = rep(1, n),
            levels = rep("TS", n),
            class = c("sample", "numeric"))
}

# Cutoff sampling
.cos <- function(x, n) {
  stopifnot("'x' must be a strictly positive and finite numeric vector" = is_positive_numeric(x))
  N <- length(x)
  if (n >= N) {
    warning("sample size is greater than or equal to population size")
    n <- min(n, N)
  }
  res <- order(x, decreasing = TRUE)[seq_len(n)]
  structure(res,
            weights = rep(1, n),
            levels = rep("TA", n),
            class = c("sample", "numeric"))
}

#---- Make a stratified sampling method ----
stratify <- function(fun) {
  fun <- match.fun(fun)
  function(x, n, s = rep(1, length(x))) {
    stopifnot("'s' must be an atomic vector" = is.atomic(s),
              "'x' and 's' must be the same length" = length(x) == length(s),
              "'n' must be a strictly positive and finite numeric vector" = is_positive_numeric(n))
    s <- as.factor(s)
    stopifnot("'n' must have a single sample size for each level in 's' (stratum)" = length(n) == nlevels(s))
    samp <- Map(fun, split(x, s), n)
    res <- Map(`[`, split(seq_along(x), s), samp)
    structure(unlist(res, use.names = FALSE),
              weights = unlist(lapply(samp, weights), use.names = FALSE),
              levels = unlist(lapply(samp, levels), use.names = FALSE),
              class = c("sample", "numeric"))
  }
}

#---- Exported sampling functions ----
sps <- stratify(.sps)

srs <- stratify(.srs)

cos <- stratify(.cos)