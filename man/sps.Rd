\name{sps}
\alias{sps}
\alias{srs}
\alias{cos}
\alias{allocate}

\title{
Sequential Poisson Sampling
}

\description{
\code{sps()} draws a probability-proportional-to-size sample according to the sequential Poisson sampling method in Ohlsson (1998). 

\code{srs()} draws a simple random sample (without replacement), and is a special case of sequential Poisson sampling.

\code{cos()} draws a cutoff sample, an alternative to sequential Poisson sampling for small or heavily skewed populations.

\code{allocate()} splits a total sample size into strata sample sizes based on size.
}

\usage{
sps(x, n, s = rep(1, length(x)))

srs(x, n, s = rep(1, length(x)))

cos(x, n, s = rep(1, length(x)))

allocate(x, n, s)
}

\arguments{
\item{x}{A positive and finite numeric vector of sizes for units in the population (e.g., revenue for drawing a sample of businesses).}

\item{n}{A positive and finite numeric vector of sample sizes for each stratum, ordered according to the levels of \code{s}.}

\item{s}{A factor, or something that can be coerced into one, giving the strata associated with \code{x}. The default is to place all units into a single stratum.}
}

\details{
The details of the sequential Poisson sampling procedure are in section 2.2 of Ohlsson (1998). Briefly, for a single stratum, all units in the population with an inclusion probability, \eqn{nx / \sum x}{nx / \sum x}, greater than or equal to 1 are placed into a take-all stratum. This process is repeated until all the inclusion probabilities are less than 1.

The remaining units in the sample belong to the take-some stratum, and are drawn by assigning each unit a value \eqn{\xi = u / x}{\xi = u / x}, where \eqn{u}{u} is from \eqn{U(0, 1)}{U(0, 1)}. The units with the smallest values for \eqn{\xi}{\xi} are included in the sample. In the unlikely event of a tie, the first unit is included in the sample. This is the same method used by PROC SURVEYSELECT in SAS with METHOD = SEQ_POISSON.

Simple random sampling is a special case of sequential Poisson sampling when \code{x} takes the same value for each unit in a stratum.

Sequential Poisson sampling may not work well in cases where there are few units in a stratum, or sizes in the stratum are concentrated among a few units. Cutoff sampling, wherein the top \code{n} units are placed in the sample, is an alternative.
}

\value{
The sampling function return an object of class \code{'sample'}. This is a numeric vector of indices for the units in the population that form the sample, along with attributes that give the design weights for each observation in the sample, and the levels (either take-all or take-some).

\code{allocate()} returns a numeric vector of length \code{nlevels(s)}.
}

\references{
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

# Equal sizes is equivalent to simple random sampling without replacement
set.seed(1234)
sps(rep(1, 10), 5)

set.seed(1234)
srs(rep(1, 10), 5)

# Sometimes cutoff sampling makes more sense than sps
cos(x, 5)

# Example of a stratified sample

strata <- rep(letters[1:4], 5)
sps(1:20, c(4, 3, 3, 2), strata)

# Allocate strata sample sizes based on size

allocate(1:20, 10, strata)
sps(1:20, allocate(1:20, 10, strata), strata)
}
