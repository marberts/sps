#' Highest-averages apportionment method
#' @noRd
highest_averages <- function(p, n, initial, available, ties, dist) {
  if (n < 0L) {
    stop("sample size must be greater than or equal to 0")
  }
  if (length(p) != length(initial)) {
    stop("initial allocation must have a single size for each stratum")
  }
  if (any(initial < 0)) {
    stop("initial allocation must be greater than or equal to 0")
  }
  if (n < sum(initial)) {
    stop("initial allocation cannot be larger than sample size")
  }
  if (length(available) != length(initial)) {
    stop(
      "number of available units in the population must be specified for ",
      "each stratum"
    )
  }
  if (n > sum(available)) {
    stop(
      "sample size cannot be greater than the number of available units in ",
      "the population"
    )
  }
  if (any(initial > available)) {
    stop(
      "initial allocation must be smaller than the number of available ",
      "units in the population for each stratum"
    )
  }
  dist <- match.fun(dist)

  res <- initial
  n <- n - sum(res)
  ord <- switch(ties,
    largest = order(p, decreasing = TRUE),
    first = seq_along(p)
  )
  p <- p[ord]
  res <- res[ord]
  available <- available[ord]
  # the while condition could be n > sum(res), but the loop below always
  # terminates after at most n steps, even if i is integer(0)
  while (n > 0L) {
    i <- which.max(p / dist(res) * (res < available))
    res[i] <- res[i] + 1L
    n <- n - 1L
  }
  res[order(ord)]
}

#' Construct a proportional allocation
#'
#' Generate a proportional-to-size allocation for stratified sampling.
#'
#' The `prop_allocation()` function gives a sample size for each level in
#' `strata` that is proportional to the sum of `x` across strata and
#' adds up to `n`. This is done using the divisor (highest-averages)
#' apportionment method (Balinksi and Young, 1982, Appendix A), for which there
#' are a number of different divisor functions:
#'
#' \describe{
#' \item{Jefferson/D'Hondt}{`\(a) a + 1`}
#' \item{Webster/Sainte-Laguë}{`\(a) a + 0.5`}
#' \item{Imperiali}{`\(a) a + 2`}
#' \item{Huntington-Hill}{`\(a) sqrt(a * (a + 1))`}
#' \item{Danish}{`\(a) a + 1 / 3`}
#' \item{Adams}{`\(a) a`}
#' \item{Dean}{`\(a) a * (a + 1) / (a + 0.5)`}
#' }
#'
#' Note that a divisor function with \eqn{d(0) = 0} (i.e., Huntington-Hill,
#' Adams, Dean) should have an initial allocation of at least 1 for all strata.
#' In all cases, ties are broken according to the sum of `x` if
#' `ties = 'largest'`; otherwise, if `ties = 'first'`, then ties are broken
#' according to the levels of `strata`.
#'
#' In cases where the number of units with non-zero size in a stratum is
#' smaller than its allocation, the allocation for that stratum is set to the
#' number of available units, with the remaining sample size reallocated to
#' other strata proportional to `x`. This is similar to \command{PROC
#' SURVEYSELECT} in SAS with \command{ALLOC = PROPORTIONAL}.
#'
#' Passing a single integer for the initial allocation first checks that
#' recycling this value for each stratum does not result in an allocation
#' larger than the sample size. If it does, then the value is reduced so that
#' recycling does not exceed the sample size. This recycled vector can be
#' further reduced in cases where it exceeds the number of units in a stratum,
#' the result of which is the initial allocation. This special recycling
#' ensures that the initial allocation is feasible.
#'
#' @inheritParams sps
#' @param n A positive integer giving the sample size.
#' @param initial A positive integer vector giving the initial (or minimal)
#' allocation for each stratum, ordered according to the levels of
#' `strata`. A single integer is recycled for each stratum using a special
#' algorithm to ensure a feasible allocation; see details. Non-integers are
#' truncated towards 0. The default allows for no units to be allocated to a
#' stratum.
#' @param divisor A divisor function for the divisor (highest-averages)
#' apportionment method. The default uses the Jefferson (D'Hondt) method. See
#' details for other possible functions.
#' @param ties Either 'largest' to break ties in favor of the stratum with the
#' largest size, or 'first' to break ties in favor of the ordering of
#' `strata`.
#'
#' @returns
#' A named integer vector of sample sizes for each stratum in `strata`.
#
#' @seealso
#' [sps()] for stratified sequential Poisson sampling.
#'
#' [expected_coverage()] to calculate the expected number of strata in a sample
#' without stratification.
#'
#' `strAlloc()` in the \pkg{PracTools} package for other allocation methods.
#'
#' @references
#' Balinksi, M. L. and Young, H. P. (1982).
#' *Fair Representation: Meeting the Ideal of One Man, One Vote*.
#' Yale University Press.
#'
#' @examples
#' # Make a population with units of different size
#' x <- c(rep(1:9, each = 3), 100, 100, 100)
#'
#' # ... and 10 strata
#' s <- rep(letters[1:10], each = 3)
#'
#' # Generate an allocation
#' prop_allocation(x, 15, s, initial = 1)
#'
#' @export
prop_allocation <- function(x,
                            n,
                            strata,
                            initial = 0L,
                            divisor = \(a) a + 1,
                            ties = c("largest", "first")) {
  x <- as.numeric(x)
  n <- as.integer(n)
  strata <- as_stratum(strata)
  initial <- as.integer(initial)
  ties <- tolower(as.character(ties))

  if (any(x < 0)) {
    stop("sizes must be greater than or equal to 0")
  }
  if (length(x) != length(strata)) {
    stop("the vectors for sizes and strata must be the same length")
  }

  x <- split(x, strata)
  ns <- vapply(x, function(x) sum(x > 0), integer(1L))

  if (length(initial) == 1L) {
    initial <- pmin.int(ns, min(n %/% nlevels(strata), initial))
  }

  p <- vapply(x, sum, numeric(1L))
  res <- highest_averages(p, n, initial, ns, match.arg(ties), divisor)
  names(res) <- levels(strata)
  res
}
