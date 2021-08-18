allocate <- function(x, N, s = rep(1L, length(x))) {
  if (not_strict_positive_vector(x)) {
    stop(gettext("'x' must be a strictly positive and finite numeric vector"))
  }
  N <- trunc(N)
  if (not_positive_number(N)) {
    stop(gettext("'N' must be a positive and finite number"))
  }
  if (N > length(x)) {
    stop(gettext("sample size 'N' is greater than or equal to population size"))
  }
  if (length(x) != length(s)) {
    stop(gettext("'x' and 's' must be the same length"))
  }
  s <- as.factor(s)
  ns <- tabulate(s)
  p <- vapply(split(x, s), sum, numeric(1)) / sum(x)
  res <- 0 # initialize result for loop
  repeat {
    res <- res + largest_remainder_round(p, N)
    d <- pmax(res - ns, 0)
    over <- as.logical(d)
    if (!any(over)) break
    res[over] <- ns[over]
    # redistribute sample units for those that cap out at the stratum size
    p[over] <- 0  
    p <- p / sum(p)
    N <- sum(d)
  }
  res
}
