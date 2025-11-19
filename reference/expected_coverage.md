# Expected coverage

Find the expected number of strata covered by ordinary Poisson sampling
without stratification. As sequential and ordinary Poisson sampling have
the same sample size on average, this gives an approximation for the
coverage under sequential Poisson sampling.

This function can also be used to calculate, e.g., the expected number
of enterprises covered within a stratum when sampling business
establishments.

## Usage

``` r
expected_coverage(x, n, strata, alpha = 0.001, cutoff = Inf)
```

## Arguments

- x:

  A positive and finite numeric vector of sizes for units in the
  population (e.g., revenue for drawing a sample of businesses).

- n:

  A positive integer giving the sample size.

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

The expected number of strata covered by the sample design.

## See also

[`prop_allocation()`](https://marberts.github.io/sps/reference/prop_allocation.md)
for generating proportional-to-size allocations.

## Examples

``` r
# Make a population with units of different size
x <- c(rep(1:9, each = 3), 100, 100, 100)

# ... and 10 strata
s <- rep(letters[1:10], each = 3)

# Should get about 7 to 8 strata in a sample on average
expected_coverage(x, 15, s)
#> [1] 7.666667
```
