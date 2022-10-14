#---- Internal functions ----
# Inclusion probability
pi <- function(x, n) {
  x / sum(x) * n
}

.inclusion_prob <- function(x, n) {
  res <- pi(x, n)
  repeat {
    to_ta <- which(res > 1)
    if (length(to_ta) == 0L) break
    res[to_ta] <- 1
    keep_ts <- which(res < 1)
    res[keep_ts] <- pi(x[keep_ts], n - length(x) + length(keep_ts))
  }
  res
}

.inclusion_prob_list <- function(x, n, s) {
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
  s <- as.factor(s)
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 's' must be the same length")
    )
  }
  if (length(n) != nlevels(s)) {
    stop(
      gettext("'n' must have a single sample size for each level in 's'")
    )
  }
  if (anyNA(s)) {
    stop(
      gettext("'s' cannot contain NAs")
    )
  }
  # bins isn't wide enough by default to catch factors with unused levels
  # at the end
  if (any(tabulate(s, nbins = nlevels(s)) < n)) {
    stop(
      gettext("sample size 'n' is greater than or equal to population size")
    )
  }
  Map(.inclusion_prob, split(x, s), n)
}

# Sequential Poisson sampling
.sps <- function(p, n, prn = NULL) {
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

# Ordinary Poisson sampling
.ps <- function(p, n, prn = NULL) {
  z <- if (is.null(prn)) runif(length(p)) else prn
  res <- which(z < p)
  weights = 1 / p[res]
  structure(
    res,
    weights = weights,
    levels = replace(rep("TA", length(res)), weights > 1, "TS"),
    class = c("sps", class(res))
  )
}

#---- Exported functions ----
# Inclusion probability
inclusion_prob <- function(x, n, s = gl(1, length(x))) {
  res <- unsplit(.inclusion_prob_list(x, n, s), s)
  attributes(res) <- NULL # unsplit() mangles attributes
  res
}

# Sampling
stratify <- function(f) {
  f <- match.fun(f)
  # return function
  function(x, n, s = gl(1, length(x)), prn = NULL) {
    n <- trunc(n)
    s <- as.factor(s)
    if (!is.null(prn)) {
      if (length(s) != length(prn)) {
        stop(
          gettext("'s' and 'prn' must be the same length")
        )
      }
      if (not_prob(prn)) {
        stop(
          gettext("'prn' must be a numeric vector between 0 and 1")
        )
      }
    }
    prn <- if (!is.null(prn)) {
      split(prn, s)
    } else {
      vector("list", nlevels(s))
    }
    samp <- Map(f, .inclusion_prob_list(x, n, s), n, prn)
    res <- Map(`[`, split(seq_along(x), s), samp)
    res <- unlist(res, use.names = FALSE) 
    weights <- unlist(lapply(samp, weights), use.names = FALSE)
    levels <- unlist(lapply(samp, levels), use.names = FALSE)
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

# Expected coverage
expected_coverage <- function(x, N, s = gl(1, length(x))) {
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
  if (N > length(x)) {
    stop(
      gettext("sample size 'N' is greater than or equal to population size")
    )
  }
  s <- as.factor(s)
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 's' must be the same length")
    )
  }
  if (anyNA(s)) {
    stop(
      gettext("'s' cannot contain NAs")
    )
  }
  p <- split(log(1 - .inclusion_prob(x, N)), s)
  sum(1 - vapply(p, function(x) exp(sum(x)), numeric(1L)))
}

# Proportional allocation
prop_allocation <- function(
    x, N, s = gl(1, length(x)), min = 0, 
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
  s <- as.factor(s)
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 's' must be the same length")
    )
  }
  if (anyNA(s)) {
    stop(
      gettext("'s' cannot contain NAs")
    )
  }
  method <- match.arg(method)
  ns <- tabulate(s, nbins = nlevels(s))
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
