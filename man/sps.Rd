\name{sps}
\alias{sps}
\alias{ps}
\alias{inclusion_prob}
\alias{prop_allocation}
\alias{expected_coverage}
\alias{weights.sps}

\title{
Stratified sequential Poisson sampling
}

\description{
Draw a stratified probability-proportional-to-size sample using the (sequential) Poisson method, with the option of an allocation proportional to size.
}

\usage{
## Sequential Poisson sampling
sps(x, n, strata = NULL, prn = NULL, alpha = 1e-4)

## Ordinary Poisson sampling
ps(x, n, strata = NULL, prn = NULL, alpha = 1e-4)

inclusion_prob(x, n, strata = NULL, alpha = 1e-4)

## Allocations
prop_allocation(x, N, strata, initial = 0, divisor = function(a) a + 1)

expected_coverage(x, N, strata, alpha = 1e-4)

\method{weights}{sps}(object, ...)
}

\arguments{
\item{x}{A positive and finite numeric vector of sizes for units in the population (e.g., revenue for drawing a sample of businesses).}

\item{n}{A positive vector of integers giving the sample size for each stratum, ordered according to the levels of \code{strata}. Non-integers are truncated towards 0.}

\item{strata}{A factor, or something that can be coerced into one, giving the strata associated with \code{x}. The default for the sampling functions is to place all units into a single stratum.}

\item{prn}{A numeric vector of permanent random numbers distributed uniform between 0 and 1, the same length as \code{x}. The default does not use permanent random numbers, instead generating a random vector when the function is called.}

\item{alpha}{A number between 0 and 1 such that units with inclusion probabilities greater than 1 - \code{alpha} are set to 1. The default is slightly larger than 0.}

\item{N}{A positive integer giving the total sample size across all strata (i.e., \code{sum(n)}). Non-integers are truncated towards 0.}

\item{initial}{A positive vector of integers giving the initial (or minimal) allocation for each stratum, ordered according to the levels of \code{strata}. A single integer is recycled for each stratum using a special algorithm to ensure a feasible allocation; see details. Non-integers are truncated towards 0. The default allows for strata with no units.}

\item{divisor}{A divisor function for the divisor (highest-averages) apportionment method. The default uses the Jefferson (D'Hondt) method. See details for other possible functions.}

\item{object}{An object of class \code{sps}, as made by \code{sps()} or \code{ps()}.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
\subsection{Sampling}{
The \code{sps()} function draws a sample according to the sequential Poisson procedure, the details of which are given by Ohlsson (1998). Briefly, for a single stratum, all units in the population with an inclusion probability, \eqn{\pi = nx / \sum x}{\pi = n * x / \sum x}, greater than or equal to \eqn{1 - \alpha} are placed into a take-all stratum. This is done by sequentially taking units with an inclusion probability greater than \eqn{1 - \alpha} (from largest to smallest) and assigning these units an inclusion probability of 1, with the remaining inclusion probabilities recalculated at each step. If \eqn{\alpha > 0}, then any ties among units with the same size are broken by their ordering in \code{x}. The inclusion probabilities for the remaining take-some units are all less than \eqn{1 - \alpha}, with all inclusion probabilities adding up to \code{n}. As noted by Ohlsson, it can be useful to set \eqn{\alpha} to a small positive value, and this is the default behavior. The \code{inclusion_prob()} function computes these stratum-wise inclusion probabilities.

A sample of take-some units is drawn by assigning each unit a value \eqn{\xi = u / \pi}, where \eqn{u} is a random deviate from the uniform distribution between 0 and 1. The units with the smallest values for \eqn{\xi} are included in the sample, along with the take-all units, resulting in a fixed sample size at the expense of the sampling procedure being only approximately probability-proportional-to-size. This is the same method used by \command{PROC SURVEYSELECT} in SAS with \command{METHOD = SEQ_POISSON}.

Ordinary Poisson sampling follows the same procedure as above, except that all units with \eqn{\xi < 1} are included in the sample; consequently, while it does not contain a fixed number of units, the procedure is strictly probability-proportional-to-size. Despite this difference, the standard Horvitz-Thompson estimator for the total is asymptotically unbiased, normally distributed, and equally efficient under both procedures. It is in this sense that sequential Poisson sampling is approximately probability-proportional-to-size. The \code{ps()} function draws a sample using the ordinary Poisson method.}

\subsection{Allocations}{
The \code{prop_allocation()} function gives a sample size for each stratum that is proportional to the sum of \code{x} across strata and adds up to \code{N}. This is done using the divisor (highest-averages) apportionment method (Balinksi and Young, 1982, Appendix A), for which there are a number of different divisor functions:

\tabular{ll}{
Jefferson/D'Hondt \tab \code{function(a) a + 1}\cr
Webster/Sainte-Laguë \tab \code{function(a) a + 0.5}\cr
Imperiali \tab \code{function(a) a + 2}\cr
Huntington–Hill \tab \code{function(a) sqrt(a * (a + 1))}\cr
Danish \tab \code{function(a) a + 1 / 3}\cr
Adams \tab \code{function(a) a}\cr
Dean \tab \code{function(a) a * (a + 1) / (a + 0.5)}
}

Note that a divisor function with \eqn{d(0) = 0} (i.e., Huntington-Hill, Adams, Dean) requires an initial allocation of at least 1 for all strata. In all cases, ties are broken according to the levels of \code{strata}; reordering the levels of \code{strata} can therefore result in a different allocation.

In cases where the number of units in a stratum is smaller than its allocation, the allocation for that stratum is set to the number of available units, with the remaining sample size reallocated to other strata proportional to \code{x}. This is similar to \command{PROC SURVEYSELECT} in SAS with \command{ALLOC = PROPORTIONAL}.

Passing a single integer for the initial allocation first checks that recycling this value for each stratum does not result in an allocation larger than the sample size. If it does, then the value is reduced so that recycling does not exceed the sample size. This recycled vector can be further reduced in cases where it exceeds the number of units in a stratum, the result of which is the initial allocation. This special recycling ensures that the initial allocation is feasible.

The \code{expected_coverage()} function gives the average number of strata covered by ordinary Poisson sampling without stratification. As sequential and ordinary Poisson sampling have the same sample size on average, this gives an approximation for the coverage under sequential Poisson sampling. This function can also be used to calculate, e.g., the expected number of enterprises covered within a stratum when sampling business establishments.}
}

\value{
\code{sps()} and \code{ps()} return an object of class \code{sps}. This is an integer vector of indices for the units in the population that form the sample, along with a \code{weights} attribute that gives the design (inverse probability) weights for each unit in the sample (keeping in mind that sequential Poisson sampling is only approximately probability-proportional-to-size), and a \code{levels} attribute that gives whether a sampled unit belongs to the take-all stratum or take-some stratum. \code{weights()} can be used to access the design weights attribute of an \code{sps} object, and \code{\link[=levels]{levels()}} can be used to access the strata. \link[=groupGeneric]{Mathematical and binary/unary operators} strip these attributes, as does replacement. 

\code{inclusion_prob()} returns a numeric vector of inclusion probabilities for each unit in the population.

\code{prop_allocation()} returns a named numeric vector of sample sizes for each stratum in \code{strata}.

\code{expected_coverage()} returns the expected number of strata covered by the sample design.
}

\references{
Balinksi, M. L. and Young, H. P. (1982). \emph{Fair Representation: Meeting the Ideal of One Man, One Vote}. Yale University Press.

Ohlsson, E. (1998). Sequential Poisson Sampling. \emph{Journal of Official Statistics}, 14(2): 149-162.
}

\seealso{
\code{\link{sps_repweights}} for generating bootstrap replicate weights.

\code{UPpoisson} and \code{inclusionprobabilities} in the \pkg{sampling} package for ordinary Poisson sampling and calculating inclusion probabilities. They are largely the same as \code{ps} and \code{inclusion_prob}, but for a single stratum.

\code{strAlloc} in the \pkg{PracTools} package for other allocation methods.

The \pkg{pps} package for other probability-proportional-to-size sampling methods.
}

\examples{
# Make a population with units of different size
x <- c(1:10, 100)

# Draw a sequential Poisson sample
(samp <- sps(x, 5))

# Get the design (inverse probability) weights
weights(samp)

# All units except 11 are in the take-some (TS) stratum
levels(samp)

# Ordinary Poisson sampling gives a random sample size for the 
# take-some stratum
ps(x, 5)

# Example of a stratified sample with a proportional allocation
strata <- rep(letters[1:4], each = 5)
(allocation <- prop_allocation(1:20, 12, strata))
sps(1:20, allocation, strata)

# It can be useful to set 'prn' in order to extend the sample
# to get a fixed net sample
u <- runif(11)
(samp <- sps(x, 6, prn = u))

# Removing unit 5 gives the same net sample
sps(x[-samp[5]], 6, prn = u[-samp[5]]) 
}
