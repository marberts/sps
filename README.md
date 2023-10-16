Sequential Poisson Sampling
================

<!-- README.md is generated from README.Rmd. Please edit that file. -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sps)](https://cran.r-project.org/package=sps)
[![sps status
badge](https://marberts.r-universe.dev/badges/sps)](https://marberts.r-universe.dev)
[![R-CMD-check](https://github.com/marberts/sps/workflows/R-CMD-check/badge.svg)](https://github.com/marberts/sps/actions)
[![codecov](https://codecov.io/gh/marberts/sps/branch/master/graph/badge.svg?token=5CPGWUF267)](https://app.codecov.io/gh/marberts/sps)

Sequential Poisson sampling is a variation of Poisson sampling for
drawing probability-proportional-to-size samples with a given number of
units, and is commonly used for price-index surveys. This package gives
functions to draw stratified sequential Poisson samples according to the
method by Ohlsson (1998), as well as other order sample designs by Rosén
(1997), and generate appropriate bootstrap replicate weights according
to the generalized bootstrap method by Beaumont and Patak (2012).

## Installation

``` r
install.packages("sps")
```

The development version can be found on GitHub.

``` r
pak::install_pkg("marberts/sps")
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
#> [1]  6  8  9 10 11 12

# Design weights and sampling strata are stored with the sample
weights(samp)
#> [1] 2.291667 1.718750 1.527778 1.375000 1.000000 1.000000
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
#> [1]  2  4  8 10 11 12

weights(samp)
#> [1] 7.500 3.750 1.875 1.000 1.000 1.000
levels(samp)
#> [1] "TS" "TS" "TS" "TA" "TA" "TA"
```

The design weights for a sample can then be used to generate bootstrap
replicate weights with the `sps_repwights()` function.

``` r
sps_repweights(weights(samp), 5, tau = 2)
#>        [,1]  [,2]   [,3]    [,4]   [,5]
#> [1,] 11.500 7.250 7.2500 15.2500 7.2500
#> [2,]  3.625 3.625 5.5000  2.2500 2.2500
#> [3,]  2.750 2.750 1.8125  2.3125 1.8125
#> [4,]  1.000 1.000 1.0000  1.0000 1.0000
#> [5,]  1.000 1.000 1.0000  1.0000 1.0000
#> [6,]  1.000 1.000 1.0000  1.0000 1.0000
#> attr(,"tau")
#> [1] 2
```

## Contribution

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
