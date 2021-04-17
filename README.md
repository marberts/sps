# sps: Sequential Poisson Sampling

[![Build
Status](https://travis-ci.com/marberts/sps.svg?branch=master)](https://travis-ci.com/marberts/sps)
[![codecov](https://codecov.io/gh/marberts/sps/branch/master/graph/badge.svg)](https://codecov.io/gh/marberts/sps)

Sequential Poisson sampling is a method for drawing probability proportional to size samples with a given number of units, and is commonly used for price-index surveys. This package gives functions to draw a stratified sequential Poisson sample according to the method in Ohlsson (1998), and generate bootstrap replicate weights according to the method in Beaumont and Patak (2012).

## Installation

```r
devtools::install_github("marberts/sps")
```

## References

Beaumont, J.-F., and Patak, Z. (2012). On the Generalized Bootstrap for Sample Surveys with Special Attention to Poisson Sampling. *International Statistical Review*, 80(1): 127-148.

Ohlsson, E. (1998). Sequential Poisson Sampling. *Journal of Official Statistics*, 14(2): 149-162.