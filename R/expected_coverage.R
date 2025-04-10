#' Expected coverage
#'
#' @description
#' Find the expected number of strata covered by ordinary Poisson sampling
#' without stratification. As sequential and ordinary Poisson sampling have the
#' same sample size on average, this gives an approximation for the coverage
#' under sequential Poisson sampling.
#'
#' This function can also be used to calculate, e.g., the expected number of
#' enterprises covered within a stratum when sampling business establishments.
#'
#' @inheritParams sps
#' @param n A positive integer giving the sample size.
#'
#' @returns
#' The expected number of strata covered by the sample design.
#'
#' @seealso
#' [prop_allocation()] for generating proportional-to-size allocations.
#'
#' @examples
#' # Make a population with units of different size
#' x <- c(rep(1:9, each = 3), 100, 100, 100)
#'
#' # ... and 10 strata
#' s <- rep(letters[1:10], each = 3)
#'
#' # Should get about 7 to 8 strata in a sample on average
#' expected_coverage(x, 15, s)
#'
#' @export
expected_coverage <- function(x, n, strata, alpha = 1e-3, cutoff = Inf) {
  x <- as.numeric(x)
  n <- as.integer(n)
  alpha <- as.numeric(alpha)
  cutoff <- as.numeric(cutoff)
  strata <- validate_strata(as.factor(strata), x)
  p <- split(log(1 - pi(x, n, alpha, cutoff)), strata)
  sum(1 - vapply(p, function(x) exp(sum(x)), numeric(1L)))
}
