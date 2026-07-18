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

  `[numeric >= 1]` A numeric vector of design (inverse probability)
  weights for a (sequential) Poisson sample.

- replicates:

  `[integer(1) >= 0]` A positive integer that gives the number of
  bootstrap replicates (1,000 by default). Non-integers are truncated
  towards 0.

- tau:

  `[numeric(1) >= 1 | function]` A number greater than or equal to 1
  that gives the rescale factor for the bootstrap weights. Setting to 1
  does not rescale the weights. This can also be a function that takes a
  vector of bootstrap adjustments and returns a number larger than 1.
  The default automatically picks the smallest feasible rescale factor
  (up to a small tolerance).

- dist:

  `[function]` A function that produces random deviates with mean 0 and
  standard deviation 1, such as
  [`rnorm()`](https://rdrr.io/r/stats/Normal.html). The default uses the
  pseudo-population method from section 4.1 of Beaumont and Patak
  (2012); see details.

- tol:

  `[0 <= numeric(1) < 1]` A non-negative number, strictly less than 1,
  that gives the tolerance for determining the minimum feasible value of
  `tau`.

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
https://doi.org/10.1111/j.1751-5823.2011.00166.x.

Ohlsson, E. (1998). Sequential Poisson Sampling. *Journal of Official
Statistics*, 14(2): 149-162.

Rosén, B. (1997). On sampling with probability proportional to size.
*Journal of Statistical Planning and Inference*, 62(2): 159-191.
https://doi.org/10.1016/S0378-3758(96)00186-3.

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
#> [1]  1  3  7 10 11

# Make some bootstrap replicates
dist <- list(
  pseudo_population = NULL,
  standard_normal = rnorm,
  exponential = \(x) rexp(x) - 1,
  uniform = \(x) runif(x, -sqrt(3), sqrt(3))
)

lapply(dist, sps_repweights, w = weights(samp), replicates = 5, tau = 2)
#> $pseudo_population
#>           [,1]     [,2]      [,3]      [,4]      [,5]
#> [1,] 7.2500000 6.750000 27.375000 6.7500000 34.750000
#> [2,] 4.8750000 2.083333  4.875000 2.0833333  4.375000
#> [3,] 0.9642857 1.946429  1.946429 0.9642857  2.928571
#> [4,] 1.7500000 0.875000  1.562500 1.5625000  0.875000
#> [5,] 1.0000000 1.000000  1.000000 1.0000000  1.000000
#> attr(,"tau")
#> [1] 2
#> 
#> $standard_normal
#>          [,1]      [,2]      [,3]       [,4]     [,5]
#> [1,] 1.958869 10.888746 30.859669 18.2278892 5.499858
#> [2,] 3.132382  3.230645  2.983794  4.1116875 3.198828
#> [3,] 2.591233  2.920457  2.368964  2.4029547 1.290134
#> [4,] 1.097756  1.702395  1.119623  0.8828485 1.208939
#> [5,] 1.000000  1.000000  1.000000  1.0000000 1.000000
#> attr(,"tau")
#> [1] 2
#> 
#> $exponential
#>           [,1]     [,2]      [,3]     [,4]      [,5]
#> [1,] 24.937910 8.253651 11.579545 9.195016 18.095020
#> [2,]  3.095485 4.271477  3.622134 2.970412  9.135676
#> [3,]  1.291456 1.417215  2.213548 1.492218  1.437055
#> [4,]  1.068304 1.080475  1.551248 1.378740  1.086677
#> [5,]  1.000000 1.000000  1.000000 1.000000  1.000000
#> attr(,"tau")
#> [1] 2
#> 
#> $uniform
#>            [,1]      [,2]      [,3]      [,4]      [,5]
#> [1,] 21.9676567 10.797790 14.732398 16.907588 15.497231
#> [2,]  4.7074432  7.517483  2.753313  3.856202  2.603224
#> [3,]  3.1068148  2.388546  1.985486  3.059590  1.072410
#> [4,]  0.7742958  1.580406  1.272099  1.124584  1.919946
#> [5,]  1.0000000  1.000000  1.000000  1.000000  1.000000
#> attr(,"tau")
#> [1] 2
#> 
```
