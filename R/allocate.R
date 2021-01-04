# I don't think this is right, but I'm not sure how PROC SURVEYSELECT with alloc=prop does it

allocate <- function(x, s, n) {
  stopifnot("'x' must be a strictly positive and finite numeric vector" = is_positive_numeric(x),
            "'s' must be an atomic vector" = is.atomic(s),
            "'x' and 's' must be the same length" = length(x) == length(s),
            "'n' must be a strictly positive and finite length 1 numeric" = is_positive_number(n),
            "'n' must be less than the length of 'x'" = n <= length(x))
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
