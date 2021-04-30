allocate <- function(x, n, s = rep(1L, length(x))) {
  if (!is_positive_numeric(x) || !all(x > 0)) {
    stop("'x' must be a strictly positive and finite numeric vector")
  }
  if (!is_positive_number(n)) {
    stop("'n' must be a positive and finite numeric vector")
  }
  if (n > length(x)) {
    stop("sample size 'n' is greater than or equal to population size")
  }
  s <- as.factor(s)
  if (length(x) != length(s)) {
    stop("'x' and 's' must be the same length")
  }
  p <- vapply(split(x, s), sum, numeric(1)) / sum(x)
  ns <- tabulate(s)
  ord <- order(ns - p * n)
  res <- numeric(length(ord))
  for (i in ord) {
    res[i] <- pmin(round(p[i] * n), ns[i])
    n <- n - res[i]
    p[i] <- 0
    p <- p / sum(p)
  }
  res
}
