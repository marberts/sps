allocate <- function(x, s, n) {
  s <- as.factor(s)
  p <- vapply(split(x, s), sum, numeric(1)) / sum(x)
  ns <- tabulate(s)
  ord <- order(ns - round(p * n, 0))
  res <- numeric(length(ord))
  for (i in ord) {
    res[i] <- pmin(round(p[i] * n, 0), ns[i])
    n <- n - res[i]
    p <- replace(p, i, 0)
    p <- p / sum(p)
  }
  res
}
