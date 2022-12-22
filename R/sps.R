#---- Internal helpers ----
# Sequential Poisson sampling
.sps <- function(p, n, u) {
  ts <- p < 1
  ta <- which(!ts)
  ts <- which(ts & p > 0)
  n_ts <- n - length(ta)
  # sample the take somes
  keep <- if (n_ts > 0) {
    # order(u[ts] / p[ts])[seq_len(n_ts)]
    partition_index(u[ts] / p[ts], n_ts)[seq_len(n_ts)]
  }
  c(ta, ts[keep])
}

# Ordinary Poisson sampling
.ps <- function(p, n, u) {
  which(u < p)
}

#---- Stratified sampling ----
stratify <- function(f) {
  f <- match.fun(f)
  # return function
  function(
    x, n, 
    strata = gl(1, length(x)), 
    prn = runif(length(x)), 
    alpha = 0
  ) {
    x <- as.numeric(x)
    if (min(x) < 0) {
      stop(gettext("'x' must be positive"))
    }
    
    n <- as.integer(n)
    if (min(n) < 0L) {
      stop(gettext("'n' must be positive"))
    }
    
    strata <- as.factor(strata)
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
    
    prn <- as.numeric(prn)
    if (length(x) != length(prn)) {
      stop(gettext("'x' and 'prn' must be the same length"))
    }
    if (min(prn) <= 0 || max(prn) >= 1) {
      stop(gettext("'prn' must be in (0, 1)"))
    }
    
    alpha <- as.numeric(alpha)
    if (alpha < 0 || alpha >= 1) {
      stop(gettext("'alpha' must be in [0, 1)"))
    }
    
    # the single-stratum case is common enough to warrant the optimization
    if (nlevels(strata) == 1L) {
      p <- .inclusion_prob(x, n, alpha)
      res <- f(p, n, prn)
      weights <- 1 / p[res]
    } else {
      p <- Map(.inclusion_prob, split(x, strata), n, alpha)
      samp <- Map(f, p, n, split(prn, strata))
      pos <- split(seq_along(x), strata)
      res <- unlist(Map(`[`, pos, samp), use.names = FALSE)
      weights <- 1 / unlist(Map(`[`, p, samp), use.names = FALSE)
    }
    ord <- order(res)
    levels <- rep("TA", length(res))
    levels[weights > 1] <- "TS"
    structure(
      res[ord],
      weights = weights[ord],
      levels = levels[ord],
      class = c("sps", class(res))
    )
  }
}

sps <- stratify(.sps)

ps <- stratify(.ps)

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
