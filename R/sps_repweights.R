#' Bootstrap replicate weights for sequential Poisson sampling
#'
#' Produce bootstrap replicate weights that are appropriate for Poisson
#' sampling, and therefore approximately correct for sequential Poisson
#' sampling.
#'
#' Replicate weights are constructed using the generalized bootstrap method by
#' Beaumont and Patak (2012). Their method takes a vector of design weights
#' \eqn{w}, finds a vector of adjustments \eqn{a} for each bootstrap replicate,
#' and calculates the replicate weights as \eqn{a w}{a * w}.
#'
#' There are two ways to calculate the adjustments \eqn{a}. The default
#' pseudo-population method randomly rounds \eqn{w} for each replicate to
#' produce a collection of integer weights \eqn{w'} that are used to generate a
#' random vector \eqn{b} from the binomial distribution. The vector of
#' adjustments is then \eqn{a = 1 + b - w' / w}. Specifying a
#' deviates-generating function for `dist` uses this function to produce a
#' random vector \eqn{d} that is then used to make an adjustment \eqn{a = 1 + d
#' \sqrt{1 - 1 / w}}{a = 1 + d * (1 - 1 / w)^0.5}.
#'
#' The adjustments can be rescaled by a value \eqn{\tau \geq 1}{\tau >= 1} to
#' prevent negative replicate weights. With this rescaling, the adjustment
#' becomes \eqn{(a + \tau - 1) / \tau}. If \eqn{\tau > 1} then the resulting
#' bootstrap variance estimator should be multiplied by \eqn{\tau^2}.
#'
#' @param w A numeric vector of design (inverse probability) weights for a
#' (sequential) Poisson sample.
#' @param replicates A positive integer that gives the number of bootstrap
#' replicates (1,000 by default). Non-integers are truncated towards 0.
#' @param tau A number greater than or equal to 1 that gives the rescale factor
#' for the bootstrap weights. Setting to 1 (the default) does not rescale the
#' weights.
#' @param dist A function that produces random deviates with mean 0 and
#' standard deviation 1, such as [rnorm()]. The default uses the
#' pseudo-population method from section 4.1 of Beaumont and Patak (2012); see
#' details.
#'
#' @returns
#' A matrix of bootstrap replicate weights with `replicates` columns (one for
#' each replicate) and `length(w)` rows (one for each unit in the sample), with
#' the value of `tau` as an attribute.
#'
#' @note
#' As an alternative to the bootstrap, Ohlsson (1998, equations 2.13)
#' proposes an analytic estimator for the variance of the total \eqn{\hat Y =
#' \sum wy}{Y = \sum w * y} (for the take-some units) under sequential Poisson
#' sampling: \deqn{V(\hat Y) = \frac{n}{n - 1} \sum \left(1 -
#' \frac{1}{w}\right) \left(wy - \frac{\hat Y}{n}\right)^2.}{V(Y) = n / (n - 1)
#' \sum (1 - 1 / w) (w * y - Y / n)^2.} See Rosén (1997, equation 3.11) for a
#' more general version of this estimator that can be applied to other order
#' sampling schemes. Replacing the left-most correction by \eqn{n / (m - 1)},
#' where \eqn{m} is the number of units in the sample, gives a similar
#' estimator for the total under ordinary Poisson sampling, \eqn{\hat Y = n / m
#' \sum wy}{Y = n / m * \sum w * y}.
#'
#' @seealso
#' [sps()] for drawing a sequential Poisson sample.
#'
#' `bootstrapFP()` (with `method = "wGeneralised"`) in the \pkg{bootstrapFP}
#' package for calculating the variance of Horvitz-Thompson estimators using
#' the generalized bootstrap.
#'
#' @references
#' Beaumont, J.-F. and Patak, Z. (2012). On the Generalized
#' Bootstrap for Sample Surveys with Special Attention to Poisson Sampling.
#' *International Statistical Review*, 80(1): 127-148.
#'
#' Ohlsson, E. (1998). Sequential Poisson Sampling.
#' *Journal of Official Statistics*, 14(2): 149-162.
#'
#' Rosén, B. (1997). On sampling with probability proportional to size.
#' *Journal of Statistical Planning and Inference*, 62(2): 159-191.
#'
#' @examples
#' # Make a population with units of different size
#' x <- c(1:10, 100)
#'
#' # Draw a sequential Poisson sample
#' (samp <- sps(x, 5))
#'
#' # Make some bootstrap replicates
#' dist <- list(
#'   pseudo_population = NULL,
#'   standard_normal = rnorm,
#'   exponential = \(x) rexp(x) - 1,
#'   uniform = \(x) runif(x, -sqrt(3), sqrt(3))
#' )
#'
#' lapply(dist, sps_repweights, w = weights(samp), replicates = 5, tau = 2)
#'
#' @export
sps_repweights <- function(w, replicates = 1000L, tau = 1, dist = NULL) {
  w <- as.numeric(w)
  if (any(w < 1)) {
    stop("design weights must be greater than or equal to 1")
  }

  replicates <- as.integer(replicates)
  if (length(replicates) > 1L || replicates < 0L) {
    stop("number of replicates must be an integer greater than or equal to 0")
  }

  tau <- as.numeric(tau)
  if (length(tau) > 1L || tau < 1) {
    stop("'tau' must be number greater than or equal to 1")
  }

  p <- 1 / w
  n <- length(w) * replicates
  a <- if (is.null(dist)) { # pseudo-population method
    wf <- floor(w)
    wr <- wf + (stats::runif(n) < w - wf)
    stats::rbinom(n, wr, p) - p * wr
  } else {
    dist <- match.fun(dist)
    dist(n) * sqrt(1 - p)
  }
  res <- w * (a + tau) / tau
  if (any(res < 0)) {
    warning("some replicate weights are negative; try increasing 'tau'")
  }
  dim(res) <- c(length(w), replicates)
  structure(res, tau = tau)
}
