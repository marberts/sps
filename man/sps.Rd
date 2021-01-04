\name{sps}
\alias{sps}
\alias{ssps}

\title{
Sequential Poisson Sampling
}

\description{
Draw a probability-proportional-to-size sample according to the sequential Poisson sampling method in Ohlsson (1998), similar to PROC SURVEYSELECT in SAS with METHOD = SEQ_POISSON.
}

\usage{
sps(x, n)

ssps(x, s, n)
}

\arguments{
\item{x}{A positive and finite numeric vector of sizes for units in the population (e.g., revenue for drawing a sample of businesses).}

\item{n}{A length 1 numeric vector giving the size of the sample. For the stratified version, this can be a vector of sample sizes for each stratum, ordered according to the levels of
code{s}.}

\item{s}{A factor, or something that can be coerced into one, giving the strata associated with \code{x}.}
}

\details{
The details of the sampling procedure are in section 2.2 of Ohlsson (1998). Briefly, all units in the population with an inclusion probability, \eqn{nx / \sum x}{nx / \sum x}, greater than or equal to 1 are placed into a take-all stratum. This process is repeated until all the inclusion probabilities are less than 1.

The remaining units in the sample belong to the take-some stratum, and are drawn by assigning each unit a value \eqn{\xi = u / x}{\xi = u / x}, where \eqn{u}{u} is from \eqn{U(0, 1)}{U(0, 1)}. The units with the smallest values for \eqn{\xi}{\xi} are included in the sample. In the unlikely event of a tie, the first unit is included in the sample.
}

\value{
A object of class \code{'sps'}. This is a numeric vector of indices for the units in the population that form the sample, along with attributes that give the design weights for each observation in the sample, and the levels (either take-all or take-some).
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

sps(rep(1, 10), 5)

# Example of a stratified sample

ssps(1:20, letters[1:2], c(4, 6))
}
