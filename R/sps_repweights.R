#---- Bootstrap replicate weights ----
sps_repweights <- function(w, B = 1000L, tau = 1, dist = NULL) {
  w <- as.numeric(w)
  B <- as.integer(B)
  tau <- as.numeric(tau)
  if (.min(w) < 1) {
    stop(gettext("'w' must be greater than 1")) 
  }
  if (B < 1L) {
    stop(gettext("'B' must be greater than 1"))
  }
  if (tau < 1) {
    stop(gettext("'tau' must be greater than 1"))
  }
  p <- 1 / w
  n <- length(w) * B
  a <- if (is.null(dist)) {
    # pseudo-population method
    wf <- floor(w)
    wr <- wf + (runif(n) < w - wf)
    rbinom(n, wr, p) - p * wr
  } else {
    dist <- match.fun(dist)
    dist(n) * sqrt(1 - p)
  }
  res <- w * (a + tau) / tau
  if (min(res) < 0) {
    warning(
      gettext("some replicate weights are negative; try increasing 'tau'")
    )
  }
  dim(res) <- c(length(w), B)
  structure(res, tau = tau)
}
