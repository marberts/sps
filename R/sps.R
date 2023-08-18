#---- Internal functions ----
# Order sampling
order_sampling_ <- function(f) {
  f <- match.fun(f)

  function(p, n, u) {
    ta <- p == 1
    ts <- which(!ta & p > 0)
    ta <- which(ta)
    n_ts <- n - length(ta)
    # sample the take somes
    keep <- if (n_ts > 0) {
      xi <- f(u[ts]) / f(p[ts])
      order(xi)[seq_len(n_ts)]
    }
    c(ta, ts[keep])
  }
}

# Ordinary Poisson sampling
ps_ <- function(p, n, u) {
  which(u < p)
}

#---- Stratified sampling ----
stratify <- function(f) {
  f <- match.fun(f)

  function(x,
           n,
           strata = gl(1, length(x)),
           prn = NULL,
           alpha = 1e-3,
           cutoff = Inf) {
    x <- as.numeric(x)
    n <- as.integer(n)
    strata <- as_stratum(strata)
    alpha <- as.numeric(alpha)
    if (is.null(prn)) {
      prn <- runif(length(x))
    } else {
      prn <- as.numeric(prn)
      if (length(x) != length(prn)) {
        stop("the vectors for sizes and permanent random numbers must be the ",
             "same length")
      }
      if (any(prn <= 0) || any(prn >= 1)) {
        stop("permanent random numbers must be in (0, 1)")
      }
    }

    p <- inclusion_prob_(x, n, strata, alpha, cutoff)
    samp <- Map(f, p, n, split(prn, strata))
    pos <- split(seq_along(prn), strata)
    # strata must have at least one level, so unlist won't return NULL
    res <- unlist(Map(`[`, pos, samp), use.names = FALSE)
    weights <- 1 / unlist(Map(`[`, p, samp), use.names = FALSE)

    ord <- order(res)
    structure(
      res[ord],
      weights = weights[ord],
      class = c("sps", "numeric")
    )
  }
}

order_sampling <- function(dist) {
  stratify(order_sampling_(dist))
}

sps <- order_sampling(identity)

ps <- stratify(ps_)

#---- Methods for class 'sps' ----
levels.sps <- function(x) {
  res <- rep.int("TS", length(x))
  res[weights(x) == 1] <- "TA"
  res
}

`levels<-.sps` <- function(x, value) {
  stop("cannot replace levels attribute")
}

`length<-.sps` <- function(x, value) {
  x <- as.vector(x)
  NextMethod()
}

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
