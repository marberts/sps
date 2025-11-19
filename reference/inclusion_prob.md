# Calculate inclusion probabilities

Calculate stratified (first-order) inclusion probabilities.

## Usage

``` r
inclusion_prob(x, n, strata = NULL, alpha = 0.001, cutoff = Inf)

becomes_ta(x, alpha = 0.001, cutoff = Inf)
```

## Arguments

- x:

  A positive and finite numeric vector of sizes for units in the
  population (e.g., revenue for drawing a sample of businesses).

- n:

  A positive integer vector giving the sample size for each stratum,
  ordered according to the levels of `strata`. A single value is
  recycled for all strata. Non-integers are truncated towards 0.

- strata:

  A factor, or something that can be coerced into one, giving the strata
  associated with units in the population. The default is to place all
  units into a single stratum.

- alpha:

  A numeric vector with values between 0 and 1 for each stratum, ordered
  according to the levels of `strata`. Units with inclusion
  probabilities greater than or equal to 1 - `alpha` are set to 1 for
  each stratum. A single value is recycled for all strata. The default
  is slightly larger than 0.

- cutoff:

  A positive numeric vector of cutoffs for each stratum, ordered
  according to the levels of `strata`. Units with `x >= cutoff` get an
  inclusion probability of 1 for each stratum. A single value is
  recycled for all strata. The default does not apply a cutoff.

## Value

`inclusion_prob()` returns a numeric vector of inclusion probabilities
for each unit in the population.

`becomes_ta()` returns an integer vector giving the sample size at which
a unit enters the take-all stratum.

## Details

Within a stratum, the inclusion probability for a unit is given by \\\pi
= nx / \sum x\\. These values can be greater than 1 in practice, and so
they are constructed iteratively by taking units with \\\pi \geq 1 -
\alpha\\ (from largest to smallest) and assigning these units an
inclusion probability of 1, with the remaining inclusion probabilities
recalculated at each step. See
[`vignette("take-all")`](https://marberts.github.io/sps/articles/take-all.md)
for details. If \\\alpha \> 0\\, then any ties among units with the same
size are broken by their position.

The `becomes_ta()` function reverses this operations and finds the
critical sample size at which a unit enters the take-all stratum. This
value is undefined for units that are always included in the sample
(because their size exceeds `cutoff`) or never included.

## Note

[`kit::topn()`](https://rdrr.io/pkg/kit/man/topn.html) is used if
available to improve performance in the normal case when the sample size
is small relative to the population.

## See also

[`sps()`](https://marberts.github.io/sps/reference/sps.md) for drawing a
sequential Poisson sample.

## Examples

``` r
# Make inclusion probabilities for a population with units
# of different size
x <- c(1:10, 100)
(pi <- inclusion_prob(x, 5))
#>  [1] 0.07272727 0.14545455 0.21818182 0.29090909 0.36363636 0.43636364
#>  [7] 0.50909091 0.58181818 0.65454545 0.72727273 1.00000000

# The last unit is sufficiently large to be included in all
# samples with two or more units
becomes_ta(x)
#>  [1] 11 11 10 10  9  9  8  8  7  7  2

# Determine the number of take-all units before drawing a sample
n_ta <- function(x, n, ...) {
  sum(becomes_ta(x, ...) <= n, na.rm = TRUE)
}

n_ta(x, 7)
#> [1] 3

# Use the inclusion probabilities to calculate the variance of the
# sample size for Poisson sampling
sum(pi * (1 - pi))
#> [1] 1.963636
```
