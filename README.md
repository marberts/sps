
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Sequential Poisson sampling <a href="https://marberts/github.io/sps/"><img src="man/figures/logo.png" align="right" height="139" alt="sps website" /></a>

[![CRAN
status](https://www.r-pkg.org/badges/version/sps)](https://cran.r-project.org/package=sps)
[![sps status
badge](https://marberts.r-universe.dev/badges/sps)](https://marberts.r-universe.dev)
[![R-CMD-check](https://github.com/marberts/sps/workflows/R-CMD-check/badge.svg)](https://github.com/marberts/sps/actions)
[![codecov](https://codecov.io/gh/marberts/sps/branch/master/graph/badge.svg?token=5CPGWUF267)](https://app.codecov.io/gh/marberts/sps)
[![DOI](https://zenodo.org/badge/326323827.svg)](https://zenodo.org/doi/10.5281/zenodo.10109857)

Sequential Poisson sampling is a variation of Poisson sampling for
drawing probability-proportional-to-size samples with a given number of
units, and is commonly used for price-index surveys. This package gives
functions to draw stratified sequential Poisson samples according to the
method by Ohlsson (1998), as well as other order sample designs by Rosén
(1997), and generate appropriate bootstrap replicate weights according
to the generalized bootstrap method by Beaumont and Patak (2012).

## Installation

Get the stable release from CRAN.

``` r
install.packages("sps")
```

The development version can be installed from R-Universe

``` r
install.packages("sps", repos = c("https://marberts.r-universe.dev", "https://cloud.r-project.org"))
```

or directly from GitHub.

``` r
pak::pkg_install("marberts/sps")
```

## Usage

Given a vector of sizes for units in a population (e.g., revenue for
sampling businesses) and a desired sample size, a stratified sequential
Poisson sample can be drawn with the `sps()` function.

``` r
library(sps)

# Generate some data on sizes for 12 businesses in a single 
# stratum as a simple example
revenue <- c(1:10, 100, 150)

# Draw a sample of 6 businesses
(samp <- sps(revenue, 6))
#> [1]  1  7  9 10 11 12

# Design weights and sampling strata are stored with the sample
weights(samp)
#> [1] 13.750000  1.964286  1.527778  1.375000  1.000000  1.000000
levels(samp)
#> [1] "TS" "TS" "TS" "TS" "TA" "TA"
```

Allocations are often proportional to size when drawing such samples,
and the `prop_allocation()` function provides a variety of methods for
generating proportional-to-size allocations.

``` r
# Add some strata
stratum <- rep(c("a", "b"), c(9, 3))

# Make an allocation
(allocation <- prop_allocation(revenue, 6, stratum))
#> a b 
#> 3 3

# Draw a stratified sample
(samp <- sps(revenue, allocation, stratum))
#> [1]  6  8  9 10 11 12

weights(samp)
#> [1] 2.500000 1.875000 1.666667 1.000000 1.000000 1.000000
levels(samp)
#> [1] "TS" "TS" "TS" "TA" "TA" "TA"
```

The design weights for a sample can then be used to generate bootstrap
replicate weights with the `sps_repweights()` function.

``` r
sps_repweights(weights(samp), 5, tau = 2)
#>          [,1]   [,2]  [,3]     [,4]      [,5]
#> [1,] 2.250000 2.2500 1.500 2.750000 3.5000000
#> [2,] 0.875000 1.8125 0.875 1.812500 2.7500000
#> [3,] 2.333333 1.5000 2.000 1.166667 0.6666667
#> [4,] 1.000000 1.0000 1.000 1.000000 1.0000000
#> [5,] 1.000000 1.0000 1.000 1.000000 1.0000000
#> [6,] 1.000000 1.0000 1.000 1.000000 1.0000000
#> attr(,"tau")
#> [1] 2
```

The vignette gives more detail about how to use these functions to draw
coordinated samples, top up a sample, and estimate variance.

## Prior work

There are a number of packages on CRAN for drawing samples proportional
to size, but these generally do not include the sequential Poisson
method. The **sampling** package contains a function for drawing
sequential Poisson samples, but it does not allow for stratification,
take-all units, or the use of permanent random numbers. By contrast, the
**prnsamplr** package allows for the use of stratification and permanent
random numbers with Pareto order sampling, but does not feature other
order-sampling methods (like sequential Poisson).

## References

Beaumont, J.-F. and Patak, Z. (2012). On the Generalized Bootstrap for
Sample Surveys with Special Attention to Poisson Sampling.
*International Statistical Review*, 80(1): 127-148.

Ohlsson, E. (1998). Sequential Poisson Sampling. *Journal of Official
Statistics*, 14(2): 149-162.

Rosén, B. (1997). On sampling with probability proportional to size.
*Journal of Statistical Planning and Inference*, 62(2): 159-191.
