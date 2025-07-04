#' Calculate inclusion probabilities
#'
#' Calculate stratified (first-order) inclusion probabilities.
#'
#' Within a stratum, the inclusion probability for a unit is given by
#' \eqn{\pi = nx / \sum x}{\pi = n * x / \sum x}. These values can be greater
#' than 1 in practice, and so they are constructed iteratively by taking units
#' with \eqn{\pi \geq 1 - \alpha}{\pi >= 1 - \alpha} (from largest to smallest)
#' and assigning these units an inclusion probability of 1, with the remaining
#' inclusion probabilities recalculated at each step. See `vignette("take-all")`
#' for details. If \eqn{\alpha > 0}, then
#' any ties among units with the same size are broken by their position.
#'
#' The `becomes_ta()` function reverses this operations and finds the critical
#' sample size at which a unit enters the take-all stratum. This value is
#' undefined for units that are always included in the sample (because their
#' size exceeds `cutoff`) or never included.
#'
#' @inheritParams sps
#'
#' @returns
#' `inclusion_prob()` returns a numeric vector of inclusion probabilities for
#' each unit in the population.
#'
#' `becomes_ta()` returns an integer vector giving the sample size at which a
#' unit enters the take-all stratum.
#'
#' @seealso
#' [sps()] for drawing a sequential Poisson sample.
#'
#' @examples
#' # Make inclusion probabilities for a population with units
#' # of different size
#' x <- c(1:10, 100)
#' (pi <- inclusion_prob(x, 5))
#'
#' # The last unit is sufficiently large to be included in all
#' # samples with two or more units
#' becomes_ta(x)
#'
#' # Use the inclusion probabilities to calculate the variance of the
#' # sample size for Poisson sampling
#' sum(pi * (1 - pi))
#'
#' @export
inclusion_prob <- function(x,
                           n,
                           strata = NULL,
                           alpha = 1e-3,
                           cutoff = Inf) {
  x <- as.numeric(x)
  n <- as.integer(n)
  alpha <- as.numeric(alpha)
  cutoff <- as.numeric(cutoff)
  if (!is.null(strata)) {
    strata <- validate_strata(as.factor(strata), x)
    unsplit(stratified_pi(x, n, strata, alpha, cutoff), strata)
  } else {
    pi(x, n, alpha, cutoff)
  }
}

#' Sample size when a unit becomes take-all
#' @rdname inclusion_prob
#' @export
becomes_ta <- function(x, alpha = 1e-3, cutoff = Inf) {
  x <- as.numeric(x)
  if (any(x < 0)) {
    stop("sizes must be greater than or equal to 0")
  }

  alpha <- as.numeric(alpha)
  if (alpha < 0 || alpha > 1) {
    stop("'alpha' must be between 0 and 1")
  }

  cutoff <- as.numeric(cutoff)
  if (cutoff <= 0) {
    stop("'cutoff' must be greater than 0")
  }

  alpha <- alpha + .Machine$double.eps^0.5
  ta <- which(x >= cutoff)
  x[ta] <- 0
  ord <- rev(order(x, decreasing = TRUE))
  x <- x[ord]
  res <- pmax.int(ceiling(cumsum(x) / x * (1 - alpha)), 1) +
    length(x) - seq_along(x) + length(ta)
  res[order(ord)]
}

#' Validate that a factor represents sampling strata
#' @noRd
validate_strata <- function(strata, x) {
  if (anyNA(strata)) {
    stop("cannot have missing strata")
  }
  if (nlevels(strata) < 1L) {
    stop("there must be at least one stratum")
  }
  if (length(x) != length(strata)) {
    stop("the vectors for sizes and strata must be the same length")
  }
  strata
}

#' Calculate unconstrained inclusion probabilities
#' @noRd
unbounded_pi <- function(x, n) {
  # n == 0 should be a strong zero.
  if (n == 0L) {
    rep.int(0, length(x))
  } else {
    x * (n / sum(x))
  }
}

#' Find the units that belong in a TA stratum
#' @noRd
ta_units <- function(x, n, alpha) {
  # Partial sorting is not stable, so if x[n] == x[n + 1] after sorting then
  # it is possible for the result to not resolve ties according to x
  # (as documented) when alpha is large enough to make at least one unit with
  # x[n] TA.
  if (n == 0L) {
    return(integer(0L))
  }
  possible_ta <- rev(topn(x, n))
  x_ta <- x[possible_ta] # ties are in reverse
  p <- x_ta * seq_len(n) / (sum(x[-possible_ta]) + cumsum(x_ta))
  # The sequence given by p has the following properties
  # 1. if p[k] < 1, then p[k + 1] >= p[k],
  # 2. if p[k] >= 1, then p[k + 1] >= 1,
  # consequently, if p[k] >= 1 - alpha, then p[k + m] >= 1 - alpha.
  possible_ta[p >= 1 - alpha]
}

#' Calculate inclusion probabilities for a single stratum
#' @noRd
pi <- function(x, n, alpha, cutoff) {
  if (any(x < 0)) {
    stop("sizes must be greater than or equal to 0")
  }
  if (n < 0L) {
    stop("sample size must be greater than or equal to 0")
  }
  if (n > sum(x > 0)) {
    stop(
      "sample size cannot be greater than the number of available units with ",
      "non-zero sizes in the population"
    )
  }
  if (alpha < 0 || alpha > 1) {
    stop("'alpha' must be between 0 and 1")
  }
  if (cutoff <= 0) {
    stop("'cutoff' must be greater than 0")
  }

  # Add some fuzz to avoid floating-point rounding stopping some units from
  # being TA, especially when alpha = 0.
  alpha <- alpha + .Machine$double.eps^0.5
  ta <- which(x >= cutoff)
  if (length(ta) > n) {
    stop("sample size is not large enough to include all units above cutoff")
  }

  x[ta] <- 0
  res <- unbounded_pi(x, n - length(ta))
  if (any(res >= 1 - alpha)) {
    ta2 <- ta_units(x, n - length(ta), alpha)
    x[ta2] <- 0
    ta <- c(ta, ta2)
    res <- unbounded_pi(x, n - length(ta))
  }

  res[ta] <- 1
  res
}

#' Calculate inclusion probabilities by stratum
#' @noRd
stratified_pi <- function(x, n, strata, alpha, cutoff) {
  if (length(n) != 1L && length(n) != nlevels(strata)) {
    stop("there must be a single sample size for each stratum")
  }
  if (length(alpha) != 1L && length(alpha) != nlevels(strata)) {
    stop("'alpha' must be a single value or have a value for each stratum")
  }
  if (length(cutoff) != 1L && length(cutoff) != nlevels(strata)) {
    stop("'cutoff' must be a single value or have a value for each stratum")
  }
  Map(pi, split(x, strata), n, alpha, cutoff)
}

#' Faster order
#' @noRd
topn <- function(x, n, decreasing = TRUE) {
  if (requireNamespace("kit", quietly = TRUE)) {
    kit::topn(x, n = n, decreasing = decreasing, hasna = FALSE)
  } else {
    order(x, decreasing = decreasing)[seq_len(n)]
  }
}
