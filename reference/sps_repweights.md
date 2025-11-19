# Bootstrap replicate weights for sequential Poisson sampling

Produce bootstrap replicate weights that are appropriate for Poisson
sampling, and therefore approximately correct for sequential Poisson
sampling.

## Usage

``` r
sps_repweights(w, replicates = 1000L, tau = min_tau(1e-04), dist = NULL)

min_tau(tol)
```

## Arguments

- w:

  A numeric vector of design (inverse probability) weights for a
  (sequential) Poisson sample.

- replicates:

  A positive integer that gives the number of bootstrap replicates
  (1,000 by default). Non-integers are truncated towards 0.

- tau:

  A number greater than or equal to 1 that gives the rescale factor for
  the bootstrap weights. Setting to 1 does not rescale the weights. This
  can also be a function that takes a vector of bootstrap adjustments
  and returns a number larger than 1. The default automatically picks
  the smallest feasible rescale factor (up to a small tolerance).

- dist:

  A function that produces random deviates with mean 0 and standard
  deviation 1, such as [`rnorm()`](https://rdrr.io/r/stats/Normal.html).
  The default uses the pseudo-population method from section 4.1 of
  Beaumont and Patak (2012); see details.

- tol:

  A non-negative number, strictly less than 1, that gives the tolerance
  for determining the minimum feasible value of `tau`.

## Value

`sps_repweights()` returns a matrix of bootstrap replicate weights with
`replicates` columns (one for each replicate) and `length(w)` rows (one
for each unit in the sample), with the value of `tau` as an attribute.

`min_tau()` returns a function that takes a vector of bootstrap
adjustments and returns the smallest value for \\\tau\\ such that the
rescaled adjustments are greater than or equal to `tol`.

## Details

Replicate weights are constructed using the generalized bootstrap method
by Beaumont and Patak (2012). Their method takes a vector of design
weights \\w\\, finds a vector of adjustments \\a\\ for each bootstrap
replicate, and calculates the replicate weights as \\a w\\.

There are two ways to calculate the adjustments \\a\\. The default
pseudo-population method randomly rounds \\w\\ for each replicate to
produce a collection of integer weights \\w'\\ that are used to generate
a random vector \\b\\ from the binomial distribution. The vector of
adjustments is then \\a = 1 + b - w' / w\\. Specifying a
deviates-generating function for `dist` uses this function to produce a
random vector \\d\\ that is then used to make an adjustment \\a = 1 + d
\sqrt{1 - 1 / w}\\.

The adjustments can be rescaled by a value \\\tau \geq 1\\ to prevent
negative replicate weights. With this rescaling, the adjustment becomes
\\(a + \tau - 1) / \tau\\. If \\\tau \> 1\\ then the resulting bootstrap
variance estimator should be multiplied by \\\tau^2\\.

## Note

As an alternative to the bootstrap, Ohlsson (1998, equations 2.13)
proposes an analytic estimator for the variance of the total \\\hat Y =
\sum wy\\ (for the take-some units) under sequential Poisson sampling:
\$\$V(\hat Y) = \frac{n}{n - 1} \sum \left(1 - \frac{1}{w}\right)
\left(wy - \frac{\hat Y}{n}\right)^2.\$\$ See Rosén (1997, equation
3.11) for a more general version of this estimator that can be applied
to other order sampling schemes. Replacing the left-most correction by
\\n / (m - 1)\\, where \\m\\ is the number of units in the sample, gives
a similar estimator for the total under ordinary Poisson sampling,
\\\hat Y = n / m \sum wy\\.

## References

Beaumont, J.-F. and Patak, Z. (2012). On the Generalized Bootstrap for
Sample Surveys with Special Attention to Poisson Sampling.
*International Statistical Review*, 80(1): 127-148.

Ohlsson, E. (1998). Sequential Poisson Sampling. *Journal of Official
Statistics*, 14(2): 149-162.

Rosén, B. (1997). On sampling with probability proportional to size.
*Journal of Statistical Planning and Inference*, 62(2): 159-191.

## See also

[`sps()`](https://marberts.github.io/sps/reference/sps.md) for drawing a
sequential Poisson sample.

`bootstrapFP()` (with `method = "wGeneralised"`) in the bootstrapFP
package for calculating the variance of Horvitz-Thompson estimators
using the generalized bootstrap and `make_gen_boot_factors()` in the
svrep package.

## Examples

``` r
# Make a population with units of different size
x <- c(1:10, 100)

# Draw a sequential Poisson sample
(samp <- sps(x, 5))
#> [1]  3  6  8 10 11

# Make some bootstrap replicates
dist <- list(
  pseudo_population = NULL,
  standard_normal = rnorm,
  exponential = \(x) rexp(x) - 1,
  uniform = \(x) runif(x, -sqrt(3), sqrt(3))
)

lapply(dist, sps_repweights, w = weights(samp), replicates = 5, tau = 2)
#> $pseudo_population
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 2.083333 9.458333 2.083333 7.166667 2.583333
#> [2,] 1.291667 2.437500 2.437500 3.083333 2.437500
#> [3,] 1.218750 2.437500 2.078125 2.437500 2.437500
#> [4,] 1.562500 0.875000 1.062500 1.062500 1.562500
#> [5,] 1.000000 1.000000 1.000000 1.000000 1.000000
#> attr(,"tau")
#> [1] 2
#> 
#> $standard_normal
#>          [,1]     [,2]      [,3]     [,4]      [,5]
#> [1,] 3.230645 2.983794 4.1116875 3.198828 6.0987420
#> [2,] 3.486979 2.797556 2.8400480 1.448908 5.2091871
#> [3,] 2.225507 1.323465 0.9569743 1.461713 2.6186964
#> [4,] 1.448742 1.944070 0.8631381 1.811052 0.7104655
#> [5,] 1.000000 1.000000 1.0000000 1.000000 1.0000000
#> attr(,"tau")
#> [1] 2
#> 
#> $exponential
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 6.749006 2.921107 5.578031 4.604439 2.956111
#> [2,] 4.044197 1.877302 3.472746 1.857871 2.567428
#> [3,] 1.257365 1.536554 1.336387 2.083488 1.362431
#> [4,] 1.319743 1.204688 1.089211 2.181617 2.292241
#> [5,] 1.000000 1.000000 1.000000 1.000000 1.000000
#> attr(,"tau")
#> [1] 2
#> 
#> $uniform
#>          [,1]     [,2]     [,3]      [,4]      [,5]
#> [1,] 3.679736 4.884021 5.549792 5.1181171 6.3946939
#> [2,] 3.537325 1.514754 1.982972 1.4510350 2.3887098
#> [3,] 2.061378 1.735871 2.603305 0.9984816 1.8088544
#> [4,] 1.580406 1.272099 1.124584 1.9199455 0.8724399
#> [5,] 1.000000 1.000000 1.000000 1.0000000 1.0000000
#> attr(,"tau")
#> [1] 2
#> 
```
