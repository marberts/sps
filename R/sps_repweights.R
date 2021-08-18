#---- Bootstrap replicate weights ----
sps_repweights <- function(w, B = 1000, tau = 1, dist = NULL) {
  if (not_positive_vector(w - 1)) {
    stop(gettext("'w' must be a finite numeric vector with each element greater than or equal to 1")) 
  }
  B <- trunc(B)
  if (not_positive_number(B - 1)) {
    stop(gettext("'B' must be a number greater than or equal to 1"))
  }
  if (not_positive_number(tau - 1)) {
    stop(gettext("'tau' must be a number greater than or equal to 1"))
  }
  pi <- 1 / w
  n <- length(w) * B
  a <- if (is.null(dist)) {
    wr <- rand_round(w, B)
    rbinom(n, wr, pi) - pi * wr
  } else {
    dist <- match.fun(dist)
    dist(n) * sqrt(1 - pi)
  }
  res <- w * (a + tau) / tau
  if (any(res < 0)) {
    warning(gettext("some replicate weights are negative; try increasing 'tau'"))
  }
  dim(res) <- c(length(w), B)
  structure(res, tau = tau, class = c("sps_brw", class(res)))
}

#---- Methods for class 'sps_brw' ----
print.sps_brw <- function(x, ...) {
  print(structure(as.numeric(x), dim = dim(x)), ...)
  invisible(x)
}