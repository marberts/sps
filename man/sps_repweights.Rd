\name{sps_repweights}
\alias{sps_repweights}

\title{
Bootstrap replicate weights for sequential Poisson sampling
}

\description{
Produce bootstrap replicate weights that are appropriate for sequential Poisson sampling according to the method in Beaumont and Patak (2012).
}

\usage{
sps_repweights(w, B = 1000, tau = 1, dist = NULL)
}

\arguments{
\item{w}{A numeric vector of design weights for a sequential Poisson sampling scheme.}

\item{B}{An integer that gives the number of bootstrap replicates (1,000 by default). Non-integers are truncated towards 0.}

\item{tau}{A number greater than or equal to 1 that gives the rescale factor for the bootstrap weights. Setting to 1 (the default) does not rescale the weights.}

\item{dist}{A function that produces random deviates with mean 0 and standard deviation 1, such as \code{\link{rnorm}}. The default uses the pseudo-population method from section 4.1 of Beaumont and Patak (2012).}
}

\details{
The details of the method for generating bootstrap replicates is in sections 4 and 6 of Beaumont and Patak (2012). Briefly, their method involves finding a vector of adjustments \eqn{a}{a} for each bootstrap replicate and calculating the replicate weights as \eqn{a w}{a * w}, where \eqn{w}{w} is a vector of design weights (inverse of the inclusion probabilities). 

The default pseudo-population method randomly rounds \eqn{w}{w} for each replicate to produce a collection of integer weights \eqn{w'}{w'} that are used to generate a random vector \eqn{b}{b} from a binomial distribution. The vector of adjustments is then \eqn{a = 1 + b - w' / w}{a = 1 + b - w' / w}. Specifying a deviates-generating function for \code{dist} will use this function to make a random vector \eqn{d}{d} that is used to make an adjustment \eqn{a = 1 + d \sqrt{1 - 1 / w}}{a = 1 + d \sqrt(1 - 1 / w)}.

These adjustments can be rescaled by a value \eqn{\tau \geq 1}{\tau \ge 1} to prevent negative replicate weights. With this rescaling, the adjustment becomes \eqn{(a + \tau - 1) / \tau}{(a + \tau - 1) / \tau}. If \eqn{\tau > 1}{\tau > 1} then the resulting bootstrap variance estimator should be multiplied by \eqn{\tau^2}{\tau^2}.
}

\value{
An object of class \code{'sps_brw'}. This is a matrix of bootstrap replicate weights with \code{B} columns (one for each replicate) and \code{length(w)} rows (one for each unit in the sample), with the value for \code{tau} as an attribute.
}

\references{
Beaumont, J.-F., and Patak, Z. (2012). On the Generalized Bootstrap for Sample Surveys with Special Attention to Poisson Sampling. \emph{International Statistical Review}, 80(1): 127-148.
}

\seealso{
\code{\link{sps}} for drawing a sequential Poisson sample.
}

\examples{
x <- c(1:10, 100) # sizes in the population

# Draw a sample
(samp <- sps(x, 5))

# Make some bootstrap replicates
sps_repweights(weights(samp), B = 5) # pseudo-population method

sps_repweights(weights(samp), B = 5, dist = rnorm) # standard-normal method

sps_repweights(weights(samp), B = 5, dist = function(x) rexp(x) - 1) # exponential method
}
