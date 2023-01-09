\name{allocations}
\alias{prop_allocation}
\alias{expected_coverage}

\title{
Proportional allocation
}

\description{
Generate a proportional-to-size allocation for stratified sampling.
}

\usage{
prop_allocation(x, N, strata, initial = 0, divisor = function(a) a + 1)

expected_coverage(x, N, strata, alpha = 1e-4)
}

\arguments{
\item{x}{A positive and finite numeric vector of sizes for units in the population (e.g., revenue for drawing a sample of businesses).}

\item{N}{A positive integer giving the total sample size across all strata. Non-integers are truncated towards 0.}

\item{strata}{A factor, or something that can be coerced into one, giving the strata associated with units in the population. The default is to place all units into a single stratum.}

\item{initial}{A positive integer vector giving the initial (or minimal) allocation for each stratum, ordered according to the levels of \code{strata}. A single integer is recycled for each stratum using a special algorithm to ensure a feasible allocation; see details. Non-integers are truncated towards 0. The default allows for strata with no units.}

\item{divisor}{A divisor function for the divisor (highest-averages) apportionment method. The default uses the Jefferson (D'Hondt) method. See details for other possible functions.}

\item{alpha}{A number between 0 and 1 such that units with inclusion probabilities greater than or equal to 1 - \code{alpha} are set to 1. The default is slightly larger than 0.}
}

\details{
The \code{prop_allocation()} function gives a sample size for each stratum that is proportional to the sum of \code{x} across strata and adds up to \code{N}. This is done using the divisor (highest-averages) apportionment method (Balinksi and Young, 1982, Appendix A), for which there are a number of different divisor functions:

\tabular{ll}{
Jefferson/D'Hondt \tab \code{\(a) a + 1}\cr
Webster/Sainte-Laguë \tab \code{\(a) a + 0.5}\cr
Imperiali \tab \code{\(a) a + 2}\cr
Huntington-Hill \tab \code{\(a) sqrt(a * (a + 1))}\cr
Danish \tab \code{\(a) a + 1 / 3}\cr
Adams \tab \code{\(a) a}\cr
Dean \tab \code{\(a) a * (a + 1) / (a + 0.5)}
}

Note that a divisor function with \eqn{d(0) = 0} (i.e., Huntington-Hill, Adams, Dean) should have an initial allocation of at least 1 for all strata. In all cases, ties are broken according to the levels of \code{strata}; reordering the levels of \code{strata} can therefore result in a different allocation.

In cases where the number of units in a stratum is smaller than its allocation, the allocation for that stratum is set to the number of available units, with the remaining sample size reallocated to other strata proportional to \code{x}. This is similar to \command{PROC SURVEYSELECT} in SAS with \command{ALLOC = PROPORTIONAL}.

Passing a single integer for the initial allocation first checks that recycling this value for each stratum does not result in an allocation larger than the sample size. If it does, then the value is reduced so that recycling does not exceed the sample size. This recycled vector can be further reduced in cases where it exceeds the number of units in a stratum, the result of which is the initial allocation. This special recycling ensures that the initial allocation is feasible.

The \code{expected_coverage()} function gives the average number of strata covered by ordinary Poisson sampling without stratification. As sequential and ordinary Poisson sampling have the same sample size on average, this gives an approximation for the coverage under sequential Poisson sampling. This function can also be used to calculate, e.g., the expected number of enterprises covered within a stratum when sampling business establishments.}

\value{
\code{prop_allocation()} returns a named numeric vector of sample sizes for each stratum in \code{strata}.

\code{expected_coverage()} returns the expected number of strata covered by the sample design.
}

\references{
Balinksi, M. L. and Young, H. P. (1982). \emph{Fair Representation: Meeting the Ideal of One Man, One Vote}. Yale University Press.
} 

\seealso{
\code{\link{sps}} for stratified sequential Poisson sampling.

\code{strAlloc} in the \pkg{PracTools} package for other allocation methods.
}

\examples{
# Make a population with units of different size
x <- c(rep(1:9, each = 3), 100, 100, 100)

# ... and 10 strata
s <- rep(letters[1:10], each = 3)

# Should get about 7 to 8 strata in a sample on average
expected_coverage(x, 15, s)

# Generate an allocation with all 10
prop_allocation(x, 15, s, initial = 1)
}
