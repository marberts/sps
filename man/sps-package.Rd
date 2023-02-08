\docType{package}
\name{sps-package}
\alias{sps-package}

\title{Sequential Poisson Sampling}

\description{
Sequential Poisson sampling is a variation of Poisson sampling for drawing probability-proportional-to-size samples with a given number of units, and is commonly used for price-index surveys. This package gives functions to draw stratified sequential Poisson samples according to the method by Ohlsson (1998), as well as other order sample designs by Rosén (1997), and generate appropriate bootstrap replicate weights according to the generalized bootstrap method by Beaumont and Patak (2012).
}

\section{Usage}{
Given a vector of sizes for units in a population (e.g., revenue for sampling businesses) and a desired sample size, a stratified sequential Poisson sample can be drawn with the \code{\link[=sps]{sps()}} function. Allocations are often proportional to size when drawing such samples, and the \code{\link[=prop_allocation]{prop_allocation()}} function provides a variety of methods for generating proportional-to-size allocations. Once the sample is drawn, the design weights for the sample can then be used to generate bootstrap replicate weights with the \code{\link[=sps_repweights]{sps_repweights()}} function.

Sequential Poisson sampling is often used to sample data for price indexes. Balk (2008, chapter 5) discusses the construction of price indexes when data are sampled using probability-proportional-to-size methods, and their resulting statistical properties. The CPI manual (2020, chapter 4) describes other methods for sampling price data. Tillé (2020, chapter 5) gives a practical overview of different probability-proportional-to-size sampling methods; compared to existing implementations of several of these methods (e.g., Brewer, Sampford, maximum entropy), however, sequential Poisson sampling is relatively fast for larger frames.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

Beaumont, J.-F. and Patak, Z. (2012). On the Generalized Bootstrap for Sample Surveys with Special Attention to Poisson Sampling. \emph{International Statistical Review}, 80(1): 127-148.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

Ohlsson, E. (1998). Sequential Poisson Sampling. \emph{Journal of Official Statistics}, 14(2): 149-162.

Rosén, B. (1997). On sampling with probability proportional to size. \emph{Journal of Statistical Planning and Inference}, 62(2): 159-191.

Tillé, Y. (2020). \emph{Sampling and estimation from finite populations}. Wiley.
}

\seealso{
\url{https://github.com/marberts/sps}
}

\author{
\strong{Maintainer}: Steve Martin \email{stevemartin041@gmail.com}

Other contributors:
\itemize{\item{Justin Francis}}
}
