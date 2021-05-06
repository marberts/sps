#---- Bootstrap replicate weights ----
sps_repweights <- function(w, B = 1000, tau = 1, dist = NULL) {
  if (!is_positive_numeric1(w)) {
    stop("'w' must be a finite numeric vector with each element greater than or equal to 1") 
  }
  if (!is_positive_number1(B)) {
    stop("'B' must be a number greater than or equal to 1")
  }
  if (!is_positive_number1(tau)) {
    stop("'tau' must be a number greater than or equal to 1")
  }
  pi <- 1 / w
  B <- trunc(B)
  n <- length(w) * B
  a <- if (is.null(dist)) {
    wr <- rand_round(w, B)
    rbinom(n, wr, pi) - pi * wr
  } else {
    dist(n) * sqrt(1 - pi)
  }
  res <- w * (a + tau) / tau
  if (any(res < 0)) warning("some replicate weights are negative; try increasing 'tau'")
  structure(res, tau = tau, dim = c(length(w), B), class = c("sps_brw", "matrix"))
}

#---- Methods for class 'sps_brw' ----
print.sps_brw <- function(x, ...) {
  print(structure(as.numeric(x), dim = dim(x)), ...)
  invisible(x)
}