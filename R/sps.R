#---- Sequential Poisson sampling ----
sps <- function(x, n) {
  stopifnot("'x' must be a strictly positive and finite numeric vector" = is_positive_numeric(x),
            "'n' must be a strictly positive and finite length 1 numeric" = is_positive_number(n),
            "'n' must be less than the length of 'x'" = n <= length(x))
  # remove the take alls so that inclusion probs are < 1
  stratum <- replace(rep("ts", length(x)), incl_prob(x, n) >= 1, "ta")
  res <- split(seq_along(x), stratum)
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
            class = c("sps", "numeric"))
}

#---- Stratified sequential Poisson sampling ----
ssps <- function(x, s, n) {
  stopifnot("'x' must be a strictly positive and finite numeric vector" = is_positive_numeric(x),
            "'s' must be an atomic vector" = is.atomic(s),
            "'x' and 's' must be the same length" = length(x) == length(s),
            "'n' must be a strictly positive and finite numeric vector" = is_positive_numeric(n),
            "Each element of 'n' must be less than the length of 'x'" = all(n <= length(x)))
  s <- as.factor(s)
  stopifnot("'n' must have a sample size for each level in 's'" = length(n) == nlevels(s))
  samp <- Map(sps, split(x, s), n)
  res <- Map(`[`, split(seq_along(x), s), samp)
  structure(unlist(res, use.names = FALSE),
            weights = unlist(lapply(samp, weights), use.names = FALSE),
            levels = unlist(lapply(samp, levels), use.names = FALSE),
            class = c("sps", "numeric"))
}

#---- Methods for class sps ----
# levels.default() extracts levels; no need for a method

weights.sps <- function(object, ...) {
  attr(object, "weights")
}

print.sps <- function(x, ...) {
  print(as.numeric(x), ...)
}
