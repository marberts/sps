\name{sps}
\alias{sps}
\alias{ps}
\alias{inclusion_prob}
\alias{order_sampling}
\alias{weights.sps}
\alias{levels.sps}

\title{
Stratified sequential Poisson sampling
}

\description{
Draw a stratified probability-proportional-to-size sample using the sequential and ordinary Poisson methods. Includes functions to calculate first-order inclusion probabilities and generate other order sampling schemes.
}

\usage{
## Sequential Poisson sampling
sps(x, n, strata = gl(1, length(x)), prn = NULL, alpha = 1e-3)

## Ordinary Poisson sampling
ps(x, n, strata = gl(1, length(x)), prn = NULL, alpha = 1e-3)

inclusion_prob(x, n, strata = gl(1, length(x)), alpha = 1e-3)

## Function factory
order_sampling(dist)
}

\arguments{
\item{x}{A positive and finite numeric vector of sizes for units in the population (e.g., revenue for drawing a sample of businesses).}

\item{n}{A positive integer vector giving the sample size for each stratum, ordered according to the levels of \code{strata}. Non-integers are truncated towards 0.}

\item{strata}{A factor, or something that can be coerced into one, giving the strata associated with units in the population. The default is to place all units into a single stratum.}

\item{prn}{A numeric vector of permanent random numbers for units in the population, distributed uniform between 0 and 1. The default does not use permanent random numbers, instead generating a random vector when the function is called.}

\item{alpha}{A numeric vector with values between 0 and 1 for each stratum, ordered according to the levels of \code{strata}. Units with inclusion probabilities greater than or equal to 1 - \code{alpha} are set to 1 for each stratum. A single value is recycled for all strata. The default is slightly larger than 0.}

\item{dist}{A function that gives the inverse of the fixed distribution shape for an order sampling scheme. See details.}
}

\details{
The \code{sps()} function draws a sample according to the sequential Poisson procedure, the details of which are given by Ohlsson (1998). It is also called uniform order sampling, as it is a type of order sampling; see Rosén (1997, 2000) for a more general presentation of the method. This is the same method used by \command{PROC SURVEYSELECT} in SAS with \command{METHOD = SEQ_POISSON}.

For each stratum, the sequential Poisson procedure starts by stratifying units in the population based on their (target) inclusion probabilities, \eqn{\pi = nx / \sum x}{\pi = n * x / \sum x}. Units with \eqn{\pi = 0} are placed into a take-none stratum, units with \eqn{0 < \pi < 1} are placed into a take-some stratum, and units with \eqn{\pi = 1} are placed into a take-all stratum. 

After units are appropriately stratified, a sample of take-some units is drawn by assigning each unit a value \eqn{\xi = u / \pi}, where \eqn{u} is a random deviate from the uniform distribution between 0 and 1. The units with the smallest values for \eqn{\xi} are included in the sample, along with the take-all units. (Ties in \eqn{\xi} are technically a measure-zero event---in practice these a broken by position.) This results in a fixed sample size at the expense of the sampling procedure being only approximately probability-proportional-to-size (i.e., the inclusion probabilities from the sample design are close but not exactly equal to \eqn{\pi}; see Matei and Tillé, 2007, for details on the exact computation).

Ordinary Poisson sampling follows the same procedure as above, except that all units with \eqn{\xi < 1} are included in the sample; consequently, while it does not contain a fixed number of units, the procedure is strictly probability-proportional-to-size. Despite this difference, the standard Horvitz-Thompson estimator for the total (of the take-some stratum) is asymptotically unbiased, normally distributed, and equally efficient under both procedures. The \code{ps()} function draws a sample using the ordinary Poisson method.

A useful feature of sequential and ordinary Poisson sampling is the ability to coordinate samples by using permanent random numbers for \eqn{u}. Keeping \eqn{u} fixed when updating a sample retains a larger number of overlapping units, whereas switching \eqn{u} for \eqn{u - z \bmod 1}{u - z mod 1} or \eqn{1 - (u - z \bmod 1)}{1 - (u - z mod 1)}, for some \eqn{z} between 0 and 1, when drawing different samples from the same frame reduces the number of overlapping units.

Inclusion probabilities can be greater than 1 in practice, and so they are constructed iteratively by taking units with \eqn{\pi \geq 1 - \alpha}{\pi >= 1 - \alpha} (from largest to smallest) and assigning these units an inclusion probability of 1, with the remaining inclusion probabilities recalculated at each step. If \eqn{\alpha > 0}, then any ties among units with the same size are broken by their position. As noted by Ohlsson, it can be useful to set \eqn{\alpha} to a small positive value, and this is the default behavior. The \code{inclusion_prob()} function computes these stratum-wise inclusion probabilities.

Despite the focus on sequential Poisson sampling, all order sampling procedures follow the same approach as sequential Poisson sampling. The \code{order_sampling()} function can be used to generate other order sampling functions by passing an appropriate function to make the ranking variable \eqn{\xi}:

\describe{
\item{Sequential Poisson sampling}{\code{\(x) x}}
\item{Successive sampling}{\code{\(x) log(1 - x)}}
\item{Pareto sampling}{\code{\(x) x / (1 - x)}}
}
}

\value{
\code{sps()} and \code{ps()} return an object of class \code{sps}. This is an integer vector of indices for the units in the population that form the sample, along with a \code{weights} attribute that gives the design (inverse probability) weights for each unit in the sample (keeping in mind that sequential Poisson sampling is only approximately probability-proportional-to-size). \code{weights()} can be used to access the design weights attribute of an \code{sps} object, and \code{levels()} can be used to determine which units are in the take-all or take-some strata. \link[=groupGeneric]{Mathematical and binary/unary operators} strip attributes, as does replacement. 

\code{inclusion_prob()} returns a numeric vector of inclusion probabilities for each unit in the population.

\code{order_sampling} returns a function the with the same interface as \code{sps()} and \code{ps()}.
}

\references{
Matei, A., and Tillé, Y. (2007). Computational aspects of order \eqn{\pi}ps sampling schemes. \emph{Computational Statistics & Data Analysis}, 51: 3703-3717.

Ohlsson, E. (1998). Sequential Poisson Sampling. \emph{Journal of Official Statistics}, 14(2): 149-162.

Rosén, B. (1997). On sampling with probability proportional to size. \emph{Journal of Statistical Planning and Inference}, 62(2): 159-191.

Rosén, B. (2000). On inclusion probabilities for order \eqn{\pi}ps sampling. \emph{Journal of Statistical Planning and Inference}, 90(1): 117-143.
} 

\seealso{
\code{\link{prop_allocation}} for generating proportional-to-size allocations.

\code{\link{sps_repweights}} for generating bootstrap replicate weights.

The \code{UPpoisson} and \code{UPopips} functions in the \pkg{sampling} package for ordinary and sequential Poisson sampling, respectively. Note that the algorithm for order sampling in the \code{UPopips} function is currently incorrect, giving a worse approximation for the inclusion probabilities than it should.

The \code{UP*} functions in the \pkg{sampling} package, the \code{S.*} functions in the \pkg{TeachingSampling} package, and the \pkg{pps} package for other probability-proportional-to-size sampling methods.

The \code{pps} function in the \pkg{prnsamplr} package for Pareto order sampling with permanent random numbers.
}

\examples{
# Make a population with units of different size
x <- c(1:10, 100)

#---- Sequential Poisson sampling ----
# Draw a sequential Poisson sample
(samp <- sps(x, 5))

# Get the design (inverse probability) weights
weights(samp)

# All units except 11 are in the take-some (TS) stratum
levels(samp)

#---- Ordinary Poisson sampling ----
# Ordinary Poisson sampling gives a random sample size for the 
# take-some stratum
ps(x, 5)

# Use the inclusion probabilities to calculate the variance of the
# sample size
with(
  list(pi = inclusion_prob(x, 5)), 
  sum(pi * (1 - pi))
)

#---- Stratified Sequential Poisson sampling ----
# Draw a stratified sample with a proportional allocation
strata <- rep(letters[1:4], each = 5)
(allocation <- prop_allocation(1:20, 12, strata))
(samp <- sps(1:20, allocation, strata))

# Use the Horvitz-Thompson estimator to estimate the total
y <- runif(20) * 1:20
sum(weights(samp) * y[samp])

#---- Useful properties of Sequential Poisson sampling ----
# It can be useful to set 'prn' in order to extend the sample
# to get a fixed net sample
u <- runif(11)
(samp <- sps(x, 6, prn = u))

# Removing unit 5 gives the same net sample
sps(x[-samp[5]], 6, prn = u[-samp[5]])

# Also useful for topping up a sample
all(samp \%in\% sps(x, 7, prn = u))

#---- Other order-sampling methods ----
# Generate new order-sampling functions from the parameters of
# the inverse generalized Pareto distribution
igpd <- function(shape, scale = 1, location = 0) {
  if (shape == 0) {
    function(x) -scale * log(1 - x) + location
  } else {
    function(x) scale * (1 - (1 - x)^shape) / shape + location
  }
}

order_sampling2 <- function(x) order_sampling(igpd(x))

order_sampling2(1)(x, 6, prn = u) # sequential Poisson
order_sampling2(0)(x, 6, prn = u) # successive
order_sampling2(-1)(x, 6, prn = u) # Pareto
}
