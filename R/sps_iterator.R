#' Iteratively draw a sequential Poisson sample
#'
#' Create a function that draws new units, one at a time, according to the
#' sequential Poisson method without replacing previously sampled units.
#'
#' @inheritParams sps
#' @param n A positive integer giving the initial sample size for the iterator.
#' @param alpha A number between 0 and 1. Units with
#'   inclusion probabilities greater than or equal to 1 - `alpha` are set to 1.
#'   The default is slightly larger than 0.
#' @param cutoff A numeric cutoff. Units with `x >= cutoff` get
#'   an inclusion probability of 1. The default does not apply a cutoff.
#'
#' @returns
#' A function that returns the next unit in the sample. It take a single
#' argument giving the sentinel value to indicate that there are no units
#' left to sample (default `NULL`).
#'
#' @examples
#' prn <- runif(5)
#' s <- sps_iterator(1:5, prn = prn)
#' s()
#' s()
#' s()
#'
#' # Same as drawing the sample with 3 units.
#' sps(1:5, 3, prn = prn)
#' @export
sps_iterator <- function(x, n = 0L, prn = NULL, alpha = 0.001, cutoff = Inf) {
  x <- as.numeric(x)
  if (any(x <= 0)) {
    stop("sizes must be strictly greater than 0")
  }
  if (is.null(prn)) {
    prn <- stats::runif(length(x))
  } else {
    prn <- as.numeric(prn)
  }
  if (length(x) != length(prn)) {
    stop("'x' and 'prn' must be the same length")
  }

  s <- order(prn / x)
  pop <- seq_along(s)
  bta <- split(
    pop,
    factor(becomes_ta(x, alpha = alpha, cutoff = cutoff), levels = pop)
  )
  if (n > 0L) {
    sampled <- as.integer(sps(x, n, prn = prn, alpha = alpha, cutoff = cutoff))
    s <- s[!s %in% sampled]
  } else {
    sampled <- integer(0L)
  }
  function(done = NULL) {
    if (length(s) == 0L) {
      return(done)
    }
    tas <- bta[[n + 1L]][!bta[[n + 1L]] %in% sampled]
    if (length(tas) > 0L) {
      res <- tas
      n <<- n + length(tas)
      s <<- s[!s %in% tas]
    } else {
      res <- s[1L]
      n <<- n + 1L
      s <<- s[-1L]
      sampled[length(sampled) + 1L] <<- res
    }
    res
  }
}
