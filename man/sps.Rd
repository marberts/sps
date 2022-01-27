\name{sps}
\alias{sps}
\alias{weights.sps}
\alias{prop_allocation}

\title{
Stratified sequential Poisson sampling
}

\description{
Draw a stratified probability-proportional-to-size sample according to the sequential Poisson sampling method by Ohlsson (1998), with the option of an allocation proportional to size.
}

\usage{
sps(x, n, s = rep(1, length(x)), prn = NULL)

\method{weights}{sps}(object, ...)

prop_allocation(x, N, s = rep(1, length(x)))
}

\arguments{
\item{x}{A strictly positive and finite numeric vector of sizes for units in the population (e.g., revenue for drawing a sample of businesses).}

\item{n}{A positive vector of integers giving the sample sizes for each stratum, ordered according to the levels of \code{s}. Non-integers are truncated towards 0.}

\item{s}{A factor, or something that can be coerced into one, giving the strata associated with \code{x}. The default is to place all units into a single stratum.}

\item{prn}{A numeric vector of permanent random numbers distributed uniform between 0 and 1, the same length as \code{x}. The default does not use permanent random numbers.}

\item{object}{An object of class \code{sps}, as made by \code{sps()}.}

\item{N}{A positive integer giving the total sample size across all strata (i.e., \code{sum(n)}). Non-integers are truncated towards 0.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The details of the sequential Poisson sampling procedure are in sections 2.2 to 2.4 of Ohlsson (1998). Briefly, for a single stratum, all units in the population with an inclusion probability, \eqn{nx / \sum x}{nx / \sum x}, greater than or equal to 1 are placed into a take-all stratum. This process is repeated until all the inclusion probabilities are less than 1.

The remaining units in the sample belong to the take-some stratum, and are drawn by assigning each unit a value \eqn{\xi = u / x}{\xi = u / x}, where \eqn{u}{u} is from \eqn{U(0, 1)}{U(0, 1)}. If \code{prn != NULL}, then the corresponding values for \code{prn} are used instead of generating random values. The units with the smallest values for \eqn{\xi}{\xi} are included in the sample. In the unlikely event of a tie, the first unit is included in the sample. This is the same method used by \command{PROC SURVEYSELECT} in SAS with \command{METHOD = SEQ_POISSON}.

The \code{prop_allocation()} function gives a sample size for each stratum that is proportional to \code{x} using largest-remainder rounding (so that they add up to \code{N}). In cases where the number of units in a stratum is smaller than its allocation, the allocation for that stratum is set to the number of available units in that stratum, with the remaining sample size reallocated to other strata proportional to \code{x}.
}

\value{
\code{sps()} returns an object of class \code{sps}. This is a numeric vector of indices for the units in the population that form the sample, along with a \code{weights} attribute that gives the design weights for each observation in the sample (keeping in mind that sequential Poisson sampling is only approximately probability-proportional-to-size), and a \code{levels} attribute that gives whether a sampled unit belongs to the take-all stratum or take-some stratum. \code{weights()} can be used to access the design weights attribute of an \code{sps} object, and \code{\link[=levels]{levels()}} can be used to access the strata.

\code{prop_allocation()} returns a named numeric vector of sample sizes for each stratum in \code{s}.
}

\references{
Ohlsson, E. (1998). Sequential Poisson Sampling. \emph{Journal of Official Statistics}, 14(2): 149-162.
}

\seealso{
\code{\link{sps_repweights}} for generating bootstrap replicate weights.

The \code{UPpoisson()} function in the \pkg{sampling} package for regular Poisson sampling.

The \pkg{pps} package for other probability-proportional-to-size sampling methods.

The \code{strAlloc()} function in the \pkg{PracTools} package for other allocation methods.
}

\examples{
x <- c(1:10, 100) # sizes in the population

# Draw a sample
(samp <- sps(x, 5))

# Get the design (inverse probability) weights
weights(samp)

# All units except 11 are in the take-some (TS) stratum
levels(samp)

# Example of a stratified sample
strata <- rep(letters[1:4], 5)
sps(1:20, c(4, 3, 3, 2), strata)

# Proportional allocation
(allocation <- prop_allocation(1:20, 12, strata))
sps(1:20, allocation, strata)

# It can be useful to set 'prn' in order to extend the sample
# to get a fixed net sample
u <- runif(11)
(samp <- sps(x, 6, prn = u))
sps(x[-samp[5]], 5, prn = u[-samp[5]]) # removing unit 5 gives the same net sample
}
