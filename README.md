
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Sequential Poisson sampling <a href="https://marberts.github.io/sps/"><img src="man/figures/logo.png" align="right" height="139" alt="sps website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sps)](https://cran.r-project.org/package=sps)
[![sps status
badge](https://marberts.r-universe.dev/badges/sps)](https://marberts.r-universe.dev)
[![Conda
Version](https://img.shields.io/conda/vn/conda-forge/r-sps.svg)](https://anaconda.org/conda-forge/r-sps)
[![R-CMD-check](https://github.com/marberts/sps/workflows/R-CMD-check/badge.svg)](https://github.com/marberts/sps/actions)
[![codecov](https://codecov.io/gh/marberts/sps/graph/badge.svg?token=5CPGWUF267)](https://app.codecov.io/gh/marberts/sps)
[![DOI](https://zenodo.org/badge/326323827.svg)](https://zenodo.org/doi/10.5281/zenodo.10109857)
[![Mentioned in Awesome Official
Statistics](https://awesome.re/mentioned-badge.svg)](https://github.com/SNStatComp/awesome-official-statistics-software)
<!-- badges: end -->

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
pak::pak("marberts/sps")
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
#> [1]  3  5  6  9 11 12

# Design weights and sampling strata are stored with the sample
weights(samp)
#> [1] 4.583333 2.750000 2.291667 1.527778 1.000000 1.000000
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
#> [1]  1  6  9 10 11 12

weights(samp)
#> [1] 15.000000  2.500000  1.666667  1.000000  1.000000  1.000000
levels(samp)
#> [1] "TS" "TS" "TS" "TA" "TA" "TA"
```

The design weights for a sample can then be used to generate bootstrap
replicate weights with the `sps_repweights()` function.

``` r
sps_repweights(weights(samp), 5)
#>          [,1]      [,2]     [,3]      [,4]         [,5]
#> [1,] 2.501250 27.498750 2.501250 27.498750 1.500000e+01
#> [2,] 0.833500  0.833500 2.083375  2.916625 2.500000e-04
#> [3,] 1.388917  2.777667 1.388917  1.388917 1.666667e-04
#> [4,] 1.000000  1.000000 1.000000  1.000000 1.000000e+00
#> [5,] 1.000000  1.000000 1.000000  1.000000 1.000000e+00
#> [6,] 1.000000  1.000000 1.000000  1.000000 1.000000e+00
#> attr(,"tau")
#> [1] 1.20012
```

The vignette gives more detail about how to use these functions to draw
coordinated samples, top up a sample, and estimate variance.

## Prior work

There are many packages on CRAN for drawing samples proportional to
size, but these generally do not include the sequential Poisson method.
The **sampling** package contains a function for drawing sequential
Poisson samples, but it does not allow for stratification, take-all
units, or the use of permanent random numbers. By contrast, the
**prnsamplr** package allows for the use of stratification and permanent
random numbers with Pareto order sampling, but does not feature other
order-sampling methods (like sequential Poisson).

## Contributing

All contributions are welcome. Please start by opening an issue on
GitHub to report any bugs or suggest improvements and new features. See
the contribution guidelines for this project for more information.

## References

Beaumont, J.-F. and Patak, Z. (2012). On the Generalized Bootstrap for
Sample Surveys with Special Attention to Poisson Sampling.
*International Statistical Review*, 80(1): 127-148.

Ohlsson, E. (1998). Sequential Poisson Sampling. *Journal of Official
Statistics*, 14(2): 149-162.

Rosén, B. (1997). On sampling with probability proportional to size.
*Journal of Statistical Planning and Inference*, 62(2): 159-191.
