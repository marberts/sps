\name{sps}
\alias{sps}
\alias{weights.sps}

\title{
Draw a sequential Poisson sample
}

\description{
Draw a probability proportional to size sample according to the sequential Poisson sampling method in Ohlsson (1998).
}

\usage{
sps(x, n, s = rep(1, length(x)))

\method{weights}{sps}(object, ...)
}

\arguments{
\item{x}{A strictly positive and finite numeric vector of sizes for units in the population (e.g., revenue for drawing a sample of businesses).}

\item{n}{A positive vector of integers giving the sample sizes for each stratum, ordered according to the levels of \code{s}. Non-integers are truncated towards 0.}

\item{s}{A factor, or something that can be coerced into one, giving the strata associated with \code{x}. The default is to place all units into a single stratum.}

\item{object}{An object of class \code{'sps'}, as made by \code{sps()}.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The details of the sequential Poisson sampling procedure are in section 2.2 of Ohlsson (1998). Briefly, for a single stratum, all units in the population with an inclusion probability, \eqn{nx / \sum x}{nx / \sum x}, greater than or equal to 1 are placed into a take-all stratum. This process is repeated until all the inclusion probabilities are less than 1.

The remaining units in the sample belong to the take-some stratum, and are drawn by assigning each unit a value \eqn{\xi = u / x}{\xi = u / x}, where \eqn{u}{u} is from \eqn{U(0, 1)}{U(0, 1)}. The units with the smallest values for \eqn{\xi}{\xi} are included in the sample. In the unlikely event of a tie, the first unit is included in the sample. This is the same method used by PROC SURVEYSELECT in SAS with METHOD = SEQ_POISSON.
}

\value{
\code{sps()} returns an object of class \code{'sps'}. This is a numeric vector of indices for the units in the population that form the sample, along with attributes that give the design weights for each observation in the sample, and the levels (either take-all or take-some).

\code{weights()} returns a numeric vector (the design weights attribute of an \code{'sps'} object).
}

\references{
Ohlsson, E. (1998). Sequential Poisson Sampling. \emph{Journal of Official Statistics}, 14(2): 149-162.
}

\seealso{
\code{\link{sps_repweights}} for generating bootstrap replicate weights.
}

\examples{
x <- c(1:10, 100) # sizes in the population

# Draw a sample
(samp <- sps(x, 5))

# Get their design (inverse probability) weights
weights(samp)

# All units except 11 are in the take-some (TS) stratum
levels(samp)

# Example of a stratified sample
strata <- rep(letters[1:4], 5)
sps(1:20, c(4, 3, 3, 2), strata)
}
