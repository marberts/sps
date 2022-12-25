#---- Bootstrap replicate weights ----
sps_repweights <- function(w, B = 1000L, tau = 1, dist = NULL) {
  w <- as.numeric(w)
  if (.min(w) < 1) {
    stop(gettext("'w' must be greater than or equal to 1")) 
  }
  
  B <- as.integer(B)
  if (B < 0L) {
    stop(gettext("'B' must be greater than or equal to 0"))
  }
  
  tau <- as.numeric(tau)
  if (tau < 1) {
    stop(gettext("'tau' must be greater than or equal to 1"))
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
  if (.min(res) < 0) {
    warning(
      gettext("some replicate weights are negative; try increasing 'tau'")
    )
  }
  dim(res) <- c(length(w), B)
  structure(res, tau = tau)
}
