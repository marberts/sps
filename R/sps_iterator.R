#' Iteratively draw a sequential Poisson sample
#'
#' Create a function that draws new units, one at a time, according to the
#' sequential Poisson method without replacing previously sampled units.
#'
#' @inheritParams sps
#' @param ... Additional arguments to [`becomes_ta()`].
#' @param n A positive integer giving the initial sample size for the iterator.
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
sps_iterator <- function(x, ..., n = 0L, prn = NULL) {
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
  bta <- split(pop, factor(becomes_ta(x, ...), levels = pop))
  if (n > 0L) {
    sampled <- as.integer(sps(x, n, prn = prn))
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
