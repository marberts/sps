prop_allocation <- function(x, N, s = rep(1L, length(x)), min = 0) {
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
  if (min * nlevels(s) > N) {
    stop(gettext("min allocation is larger than 'N'"))
  } 
  p <- vapply(split(x, s), sum, numeric(1L))
  res <- structure(rep(min, length(p)), names = levels(s)) # initialize result for loop
  repeat {
    res <- largest_remainder_round(p, N, res)
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
