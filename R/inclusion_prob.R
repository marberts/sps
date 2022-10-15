#---- Internal helpers ----
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

#---- Inclusion probability ----
inclusion_prob <- function(x, n, s = gl(1, length(x))) {
  s <- as.factor(s)
  res <- unsplit(.inclusion_prob_list(x, n, s), s)
  attributes(res) <- NULL # unsplit() mangles attributes
  res
}
