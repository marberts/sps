largest_remainder_rounding <- function(n, p) {
  np <- n * p
  npf <- floor(np)
  npf + (rank(npf - np, ties.method = "first") <= n - sum(npf))
}

allocate <- function(x, n, s = rep(1L, length(x))) {
  if (!is_positive_numeric(x) || !all(x > 0)) {
    stop("'x' must be a strictly positive and finite numeric vector")
  }
  if (!is_positive_number(n)) {
    stop("'n' must be a positive and finite number")
  }
  if (n > length(x)) {
    stop("sample size 'n' is greater than or equal to population size")
  }
  if (length(x) != length(s)) {
    stop("'x' and 's' must be the same length")
  }
  s <- as.factor(s)
  ns <- tabulate(s)
  p <- vapply(split(x, s), sum, numeric(1)) / sum(x)
  res <- 0
  repeat {
    res <- res + largest_remainder_rounding(n, p)
    d <- pmax(res - ns, 0)
    over <- as.logical(d)
    if (!any(over)) break
    p[over] <- 0
    p <- p / sum(p)
    res[over] <- ns[over]
    n <- sum(d)
  }
  res
}
