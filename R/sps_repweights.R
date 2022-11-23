#---- Internal helpers ----
random_round <- function(x, n) {
  x <- as.numeric(x) # dims cause problems
  y <- floor(x)
  y + (runif(length(x) * n) < x - y)
}

#---- Bootstrap replicate weights ----
sps_repweights <- function(w, B = 1000, tau = 1, dist = NULL) {
  if (not_positive_vector(w - 1)) {
    stop(
      gettext("'w' must be a finite numeric vector with each element greater than or equal to 1")
    ) 
  }
  B <- trunc(B)
  if (not_positive_number(B - 1)) {
    stop(
      gettext("'B' must be a number greater than or equal to 1")
    )
  }
  if (not_positive_number(tau - 1)) {
    stop(
      gettext("'tau' must be a number greater than or equal to 1")
    )
  }
  p <- 1 / w
  n <- length(w) * B
  a <- if (is.null(dist)) {
    # pseudo-population method
    wr <- random_round(w, B)
    rbinom(n, wr, p) - p * wr
  } else {
    dist <- match.fun(dist)
    dist(n) * sqrt(1 - p)
  }
  res <- as.numeric(w * (a + tau) / tau) # strip attributes
  if (min(res) < 0) {
    warning(
      gettext("some replicate weights are negative; try increasing 'tau'")
    )
  }
  dim(res) <- c(length(w), B)
  rownames(res) <- names(w)
  structure(res, tau = tau, class = c("sps_brw", class(res)))
}

#---- Methods for class 'sps_brw' ----
print.sps_brw <- function(x, ...) {
  print(structure(as.vector(x), dim = dim(x), dimnames = dimnames(x)), ...)
  invisible(x)
}
