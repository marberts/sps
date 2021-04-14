\name{sps}
\alias{sps}
\alias{weights.sps}
\alias{rep.sps}

\title{
Sequential Poisson Sampling
}

\description{
\code{sps()} draws a probability-proportional-to-size sample according to the sequential Poisson sampling method in Ohlsson (1998). \code{weights()} extracts design weights for the sample, and \code{rep()} produces bootstrap replicates according to the method in Beaumont and Patak (2012).
}

\usage{
sps(x, n, s = rep(1, length(x)))

\method{weights}{sps}(object, ...)

\method{rep}{sps}(x, B = 1000, tau = 1, dist = NULL, ...)
}

\arguments{
\item{x}{A positive and finite numeric vector of sizes for units in the population (e.g., revenue for drawing a sample of businesses).}

\item{n}{A positive and finite numeric vector of sample sizes for each stratum, ordered according to the levels of \code{s}.}

\item{s}{A factor, or something that can be coerced into one, giving the strata associated with \code{x}. The default is to place all units into a single stratum.}

\item{object}{An object of class \code{'sps'}, as made by \code{sps()}.}

\item{B}{The number of bootstrap replicates (1,000 by default).}

\item{tau}{The rescale factor for the bootstrap weights. Setting to 1 (the default) does not rescale the weights.}

\item{dist}{A function that produces random deviates with mean 0 and standard deviation 1, such as \code{\link{rnorm}}. The default uses the pseudo-population method from section 4.1 of Beaumont and Patak (2012).}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The details of the sequential Poisson sampling procedure are in section 2.2 of Ohlsson (1998). Briefly, for a single stratum, all units in the population with an inclusion probability, \eqn{nx / \sum x}{nx / \sum x}, greater than or equal to 1 are placed into a take-all stratum. This process is repeated until all the inclusion probabilities are less than 1.

The remaining units in the sample belong to the take-some stratum, and are drawn by assigning each unit a value \eqn{\xi = u / x}{\xi = u / x}, where \eqn{u}{u} is from \eqn{U(0, 1)}{U(0, 1)}. The units with the smallest values for \eqn{\xi}{\xi} are included in the sample. In the unlikely event of a tie, the first unit is included in the sample. This is the same method used by PROC SURVEYSELECT in SAS with METHOD = SEQ_POISSON.

The details of the method for generating bootstrap replicates is in sections 4 and 6 of Beaumont and Patak (2012). Briefly, their method involves finding a vector of adjustments \eqn{a}{a} for each replicate and calculating the replicate weights as \eqn{a w}{a * w}, where \eqn{w}{w} is the inverse of the inclusion probabilities (design weights). 

The default pseudo-population method randomly rounds \eqn{w}{w} for each replicate to produce a collection of integer weights \eqn{w'}{w'} that are used to generate a random vector \eqn{b}{b} from a binomial distribution. The vector of adjustments is then \eqn{a = 1 + b - w' / w}{a = 1 + b - w' / w}. Specifying a deviates-generating function for \code{dist} will use this function to make a random vector \eqn{d}{d} that is used to make an adjustment \eqn{a = 1 + d \sqrt{1 - 1 / w}}{a = 1 + d \sqrt(1 - 1 / w)}.

These adjustments can be rescaled by a value \eqn{\tau \geq 1}{\tau \ge 1} to prevent replicate weights that are less than 1. With this rescaling, the adjustment becomes \eqn{(a + \tau - 1) / \tau}{(a + \tau - 1) / \tau}. If \eqn{\tau > 1}{\tau > 1} then the resulting boostrap variance estimator should be multiplied by \eqn{\tau^2}{\tau^2}.
}

\value{
\code{sps()} returns an object of class \code{'sps'}. This is a numeric vector of indices for the units in the population that form the sample, along with attributes that give the design weights for each observation in the sample, and the levels (either take-all or take-some).

\code{weights()} returns a numeric vector (the design weights attribute of an \code{'sps'} object).

\code{rep()} returns a matrix with \code{B} columns (one for each replicate) and \code{length(x)} rows (one for each unit in the sample).
}

\references{
Beaumont, J.-F., and Patak, Z. (2012). On the Generalized Bootstrap for Sample Surveys with Special Attention to Poisson Sampling. \emph{International Statistical Review}, 80(1): 127-148.

Ohlsson, E. (1998). Sequential Poisson Sampling. \emph{Journal of Official Statistics}, 14(2): 149-162.
}

\examples{
x <- c(1:10, 100)
samp <- sps(x, 5)

# Which units are in the sample?
samp

# Get their inverse-probability weights
weights(samp)

# All units except 11 are in the take-some (TS) stratum
levels(samp)

# Make some bootstrap replicates
rep(samp, B = 5) # pseudo-population method

rep(samp, B = 5, dist = rnorm) # standard-normal method

# Example of a stratified sample
strata <- rep(letters[1:4], 5)
sps(1:20, c(4, 3, 3, 2), strata)
}
