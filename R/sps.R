#---- Sequential Poisson sampling (internal) ----
.sps <- function(x, n, prn = NULL) {
  p <- inclusion_prob(x, n)
  ts <- p < 1
  ta <- which(!ts)
  ts <- which(ts)
  n_ts <- n - length(ta)
  # sample the take somes
  keep <- if (n_ts > 0) {
    z <- if (is.null(prn)) runif(length(ts)) else prn[ts]
    order(z / p[ts])[seq_len(n_ts)]
  }
  res <- c(ta, ts[keep])
  structure(
    res,
    weights = 1 / p[res],
    levels = rep(c("TA", "TS"), c(n - n_ts, n_ts)),
    class = c("sps", class(res))
  )
}

#---- Ordinary Poisson sampling (internal) ----
.ps <- function(x, n, prn = NULL) {
  N <- length(x)
  p <- inclusion_prob(x, n)
  z <- if (is.null(prn)) runif(N) else prn
  res <- which(z < p)
  structure(
    res,
    weights = 1 / p[res],
    levels = replace(rep("TA", N), p[res] < 1, "TS"),
    class = c("sps", class(res))
  )
}

#---- Stratified sampling (exported) ----
stratify <- function(f) {
  f <- match.fun(f)
  # return function
  function(x, n, s = rep(1L, length(x)), prn = NULL) {
    if (not_strict_positive_vector(x)) {
      stop(
        gettext("'x' must be a strictly positive and finite numeric vector")
      )
    }
    n <- trunc(n)
    if (not_positive_vector(n)) {
      stop(
        gettext("'n' must be a positive and finite numeric vector")
      )
    }
    if (length(x) != length(s)) {
      stop(
        gettext("'x' and 's' must be the same length")
      )
    }
    s <- as.factor(s)
    if (length(n) != nlevels(s)) {
      stop(
        gettext("'n' must have a single sample size for each level in 's'")
      )
    }
    if (any(tabulate(s) < n)) {
      stop(
        gettext("sample size 'n' is greater than or equal to population size")
      )
    }
    if (!is.null(prn)) {
      if (length(x) != length(prn)) {
        stop(
          gettext("'x' and 'prn' must be the same length")
        )
      }
      if (not_prob(prn)) {
        stop(
          gettext("'prn' must be a numeric vector between 0 and 1")
        )
      }
    }
    prn <- if (!is.null(prn)) split(prn, s) else vector("list", nlevels(s))
    samp <- .mapply(f, list(split(x, s), n, prn), list())
    res <- .mapply(`[`, list(split(seq_along(x), s), samp), list())
    res <- unlist(res, use.names = FALSE) 
    if (!length(res)) res <- integer(0L) # unlist can return NULL
    weights <- as.numeric(unlist(lapply(samp, weights), use.names = FALSE))
    levels <- as.character(unlist(lapply(samp, levels), use.names = FALSE))
    ord <- order(res)
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

#---- Coverage ----
sps_coverage <- function(x, n, g) {
  p <- split(inclusion_prob(x, n), g)
  sum(1 - vapply(p, function(x) prod(1 - x), numeric(1)))
}

#---- Proportional allocation ----
prop_allocation <- function(
    x, N, s = rep(1L, length(x)), min = 0, 
    method = c("Largest-remainder", "D'Hondt", "Webster", "Imperiali", 
               "Huntington-Hill", "Danish", "Adams", "Dean")  
) {
  if (not_strict_positive_vector(x)) {
    stop(
      gettext("'x' must be a strictly positive and finite numeric vector")
    )
  }
  N <- trunc(N)
  if (not_positive_number(N)) {
    stop(
      gettext("'N' must be a positive and finite number")
    )
  }
  min <- trunc(min)
  if (not_positive_number(min)) {
    stop(
      gettext("'min' must be a positive and finite number")
    )
  }
  if (N > length(x)) {
    stop(
      gettext("sample size 'N' is greater than or equal to population size")
    )
  }
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 's' must be the same length")
    )
  }
  method <- match.arg(method)
  s <- as.factor(s)
  ns <- tabulate(s)
  if (any(min > ns)) {
    stop(
      gettext("'min' must be smaller than the population size for each stratum")
    )
  }
  if ((N <- N - min * nlevels(s)) < 0) {
    stop(
      gettext("minimal allocation is larger than 'N'")
    )
  }
  # apportionment method
  round <- if (method == "Largest-remainder") {
    largest_remainder
  } else {
    highest_averages(method)
  }
  p <- vapply(split(x, s), sum, numeric(1L))
  res <- structure(rep(min, length(p)), names = levels(s))
  # redistribute sample units for those that cap out at the stratum size
  repeat {
    res <- round(p, N, res)
    d <- pmax(res - ns, 0)
    over <- as.logical(d)
    if (!any(over)) break
    res[over] <- ns[over]
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
