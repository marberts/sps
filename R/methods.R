#---- Methods for class 'sps' ----
# levels.default() extracts levels; no need for a method

weights.sps <- function(object, ...) {
  attr(object, "weights")
}

print.sps <- function(x, ...) {
  print(as.numeric(x), ...)
}

rep.sps <- function(x, B = 1000, tau = 1, dist = NULL, ...) {
  if (!is_positive_number1(B)) stop("'B' must be a number >= 1")
  if (!is_positive_number1(tau)) stop("'tau' must be a number >= 1")
  w <- weights(x)
  pi <- 1 / w
  B <- trunc(B)
  n <- length(x) * B
  a <- if (is.null(dist)) {
    wr <- rep(floor(w), B)
    wr <- wr + (runif(n) < w - wr)
    rbinom(n, wr, pi) - pi * wr
  } else {
    dist(n) * sqrt(1 - pi)
  }
  res <- w * (a + tau) / tau
  if (any(res < 1)) warning("some replicate weights are < 1; try increasing 'tau'")
  structure(res, tau = tau, dim = c(length(w), B))
}
