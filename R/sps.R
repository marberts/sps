#---- Sequential Poisson sampling (internal) ----
.sps <- function(x, n, prn = NULL) {
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
    z <- if (is.null(prn)) runif(length(res$ts)) else prn[res$ts]
    order(z / p)[seq_len(n_ts)]
  }
  res$ts <- res$ts[keep]
  res <- unlist(res, use.names = FALSE) # unlist can return NULL
  if (!length(res)) res <- integer(0L)
  structure(res,
            weights = c(rep(1, n - n_ts), 1 / p[keep]),
            levels = rep(c("TA", "TS"), c(n - n_ts, n_ts)),
            class = c("sps", class(res)))
}

#---- Stratified sequential Poisson sampling (exported) ----
sps <- function(x, n, s = rep(1L, length(x)), prn = NULL) {
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
  if (!is.null(prn)) {
    if (length(x) != length(prn)) {
      stop(gettext("'x' and 'prn' must be the same length"))
    }
    if (not_prob(prn)) {
      stop(gettext("'prn' must be a numeric vector between 0 and 1"))
    }
  }
  prn <- if (!is.null(prn)) split(prn, s) else vector("list", nlevels(s))
  samp <- .mapply(.sps, list(split(x, s), n, prn), list())
  res <- .mapply(`[`, list(split(seq_along(x), s), samp), list())
  res <- unlist(res, use.names = FALSE) 
  if (!length(res)) res <- integer(0L) # unlist can return NULL
  structure(res,
            weights = as.numeric(unlist(lapply(samp, weights), use.names = FALSE)),
            levels = as.character(unlist(lapply(samp, levels), use.names = FALSE)),
            class = c("sps", class(res)))
}

#---- Proportional allocation ----
prop_allocation <- function(x, N, s = rep(1L, length(x)), min = 0, method = "Largest-remainder") {
  if (not_strict_positive_vector(x)) {
    stop(gettext("'x' must be a strictly positive and finite numeric vector"))
  }
  N <- trunc(N)
  if (not_positive_number(N)) {
    stop(gettext("'N' must be a positive and finite number"))
  }
  min <- trunc(min)
  if (not_positive_number(min)) {
    stop(gettext("'min' must be a positive and finite number"))
  }
  if (N > length(x)) {
    stop(gettext("sample size 'N' is greater than or equal to population size"))
  }
  if (length(x) != length(s)) {
    stop(gettext("'x' and 's' must be the same length"))
  }
  s <- as.factor(s)
  ns <- tabulate(s)
  if (any(min > ns)) {
    stop(gettext("'min' must be smaller than the population size for each stratum"))
  }
  if ((N <- N - min * nlevels(s)) < 0) {
    stop(gettext("minimal allocation is larger than 'N'"))
  }
  round <- apportionment(method)
  p <- vapply(split(x, s), sum, numeric(1L))
  res <- structure(rep(min, length(p)), names = levels(s))
  repeat {
    res <- round(p, N, res)
    d <- pmax(res - ns, 0)
    over <- as.logical(d)
    if (!any(over)) break
    res[over] <- ns[over]
    # redistribute sample units for those that cap out at the stratum size
    p[over] <- 0  
    N <- sum(d)
  }
  res
}

#---- Methods for class 'sps' ----
# levels.default() extracts levels; no need for a method

weights.sps <- function(object, ...) {
  attr(object, "weights")
}

print.sps <- function(x, ...) {
  print(as.vector(x), ...)
  invisible(x)
}

Math.sps <- function(x, ...) {
  x <- as.vector(x)
  NextMethod()
}

Ops.sps <- function(e1, e2) {
  if (inherits(e1, "sps")) e1 <- as.vector(e1)
  if (nargs() == 2L && inherits(e2, "sps")) e2 <- as.vector(e2)
  NextMethod()
}

`[<-.sps` <- `[[<-.sps` <- function(x, i, value) {
  x <- as.vector(x)
  NextMethod()
}
