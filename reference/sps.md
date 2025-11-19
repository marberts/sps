# Stratified sequential Poisson sampling

Draw a stratified probability-proportional-to-size sample using the
sequential and ordinary Poisson methods, and generate other order
sampling schemes.

## Usage

``` r
sps(x, n, strata = NULL, prn = NULL, alpha = 0.001, cutoff = Inf)

ps(x, n, strata = NULL, prn = NULL, alpha = 0.001, cutoff = Inf)

order_sampling(dist)
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

- prn:

  A numeric vector of permanent random numbers for units in the
  population, distributed uniform between 0 and 1. The default does not
  use permanent random numbers, instead generating a random vector when
  the function is called.

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

- dist:

  A function giving the fixed order distribution shape for an order
  sampling scheme. See details.

## Value

`sps()` and `ps()` return an object of class `sps_sample`. This is an
integer vector of indices for the units in the population that form the
sample, along with a `weights` attribute that gives the design (inverse
probability) weights for each unit in the sample (keeping in mind that
sequential Poisson sampling is only approximately
probability-proportional-to-size).
[`weights()`](https://rdrr.io/r/stats/weights.html) can be used to
access the design weights attribute of an `sps_sample` object, and
[`levels()`](https://rdrr.io/r/base/levels.html) can be used to
determine which units are in the take-all or take-some strata.
[Mathematical and binary/unary
operators](https://rdrr.io/r/base/groupGeneric.html) strip attributes,
as does replacement.

`order_sampling` returns a function the with the same interface as
`sps()` and `ps()`.

## Details

The `sps()` function draws a sample according to the sequential Poisson
procedure, the details of which are given by Ohlsson (1998). It is also
called uniform order sampling, as it is a type of order sampling; see
Rosén (1997, 2000) for a more general presentation of the method. This
is the same method used by `PROC SURVEYSELECT` in SAS with
`METHOD = SEQ_POISSON`.

For each stratum, the sequential Poisson procedure starts by stratifying
units in the population based on their (target) inclusion probabilities
\\\pi\\. Units with \\\pi = 0\\ are placed into a take-none stratum,
units with \\0 \< \pi \< 1\\ are placed into a take-some stratum, and
units with \\\pi = 1\\ are placed into a take-all stratum. As noted by
Ohlsson (1998), it can be useful to set \\\alpha\\ to a small positive
value when calculating inclusion probabilities, and this is the default
behavior.

After units are appropriately stratified, a sample of take-some units is
drawn by assigning each unit a value \\\xi = u / \pi\\, where \\u\\ is a
random deviate from the uniform distribution between 0 and 1. The units
with the smallest values for \\\xi\\ are included in the sample, along
with the take-all units. (Ties in \\\xi\\ are technically a measure-zero
event—in practice these are broken by position.) This results in a fixed
sample size at the expense of the sampling procedure being only
approximately probability-proportional-to-size (i.e., the inclusion
probabilities from the sample design are close but not exactly equal to
\\\pi\\; see Matei and Tillé, 2007, for details on the exact
computation).

Ordinary Poisson sampling follows the same procedure as above, except
that all units with \\\xi \< 1\\ are included in the sample;
consequently, while it does not contain a fixed number of units, the
procedure is strictly probability-proportional-to-size. Despite this
difference, the standard Horvitz-Thompson estimator for the total (of
the take-some stratum) is asymptotically unbiased, normally distributed,
and equally efficient under both procedures. The `ps()` function draws a
sample using the ordinary Poisson method.

A useful feature of sequential and ordinary Poisson sampling is the
ability to coordinate samples by using permanent random numbers for
\\u\\. Keeping \\u\\ fixed when updating a sample retains a larger
number of overlapping units, whereas switching \\u\\ for \\u - z \bmod
1\\ or \\1 - (u - z \bmod 1)\\, for some \\z\\ between 0 and 1, when
drawing different samples from the same frame reduces the number of
overlapping units.

Despite the focus on sequential Poisson sampling, all order sampling
procedures follow the same approach as sequential Poisson sampling. The
`order_sampling()` function can be used to generate other order sampling
functions by passing an appropriate function to make the ranking
variable \\\xi\\:

- Sequential Poisson sampling:

  `\(x) x`

- Successive sampling:

  `\(x) log(1 - x)`

- Pareto sampling:

  `\(x) x / (1 - x)`

## Note

[`kit::topn()`](https://rdrr.io/pkg/kit/man/topn.html) is used if
available to improve performance in the normal case when the sample size
is small relative to the population.

## References

Matei, A., and Tillé, Y. (2007). Computational aspects of order
\\\pi\\ps sampling schemes. *Computational Statistics & Data Analysis*,
51: 3703-3717.

Ohlsson, E. (1998). Sequential Poisson Sampling. *Journal of Official
Statistics*, 14(2): 149-162.

Rosén, B. (1997). On sampling with probability proportional to size.
*Journal of Statistical Planning and Inference*, 62(2): 159-191.

Rosén, B. (2000). On inclusion probabilities for order \\\pi\\ps
sampling. *Journal of Statistical Planning and Inference*, 90(1):
117-143.

## See also

[`prop_allocation()`](https://marberts.github.io/sps/reference/prop_allocation.md)
for generating proportional-to-size allocations.

[`inclusion_prob()`](https://marberts.github.io/sps/reference/inclusion_prob.md)
for calculating the inclusion probabilities.

[`sps_repweights()`](https://marberts.github.io/sps/reference/sps_repweights.md)
for generating bootstrap replicate weights.

The `UPpoisson()` and `UPopips()` functions in the sampling package for
ordinary and sequential Poisson sampling, respectively. Note that the
algorithm for order sampling in the `UPopips()` function is currently
incorrect, giving a worse approximation for the inclusion probabilities
than it should.

The `UP*` functions in the sampling package, the `S.*` functions in the
TeachingSampling package, and the pps package for other
probability-proportional-to-size sampling methods.

The `pps()` function in the prnsamplr package for Pareto order sampling
with permanent random numbers.

## Examples

``` r
# Make a population with units of different size
x <- c(1:10, 100)

#---- Sequential Poisson sampling ----
# Draw a sequential Poisson sample
(samp <- sps(x, 5))
#> [1]  4  5  8 10 11

# Get the design (inverse probability) weights
weights(samp)
#> [1] 3.43750 2.75000 1.71875 1.37500 1.00000

# All units except 11 are in the take-some (TS) stratum
levels(samp)
#> [1] "TS" "TS" "TS" "TS" "TA"

# Ensure that the top 10% of units are in the sample
sps(x, 5, cutoff = quantile(x, 0.9))
#> [1]  5  8  9 10 11

#---- Ordinary Poisson sampling ----
# Ordinary Poisson sampling gives a random sample size for the
# take-some stratum
ps(x, 5)
#> [1]  1  6  9 10 11

#---- Stratified Sequential Poisson sampling ----
# Draw a stratified sample with a proportional allocation
strata <- rep(letters[1:4], each = 5)
(allocation <- prop_allocation(1:20, 12, strata))
#> a b c d 
#> 1 2 4 5 
(samp <- sps(1:20, allocation, strata))
#>  [1]  4  9 10 11 12 13 14 16 17 18 19 20

# Use the Horvitz-Thompson estimator to estimate the total
y <- runif(20) * 1:20
sum(weights(samp) * y[samp])
#> [1] 92.70057

#---- Useful properties of Sequential Poisson sampling ----
# It can be useful to set 'prn' in order to extend the sample
# to get a fixed net sample
u <- runif(11)
(samp <- sps(x, 6, prn = u))
#> [1]  3  6  7  8  9 11

# Removing unit 5 gives the same net sample
sps(x[-samp[5]], 6, prn = u[-samp[5]])
#> [1]  3  6  7  8  9 10

# Also useful for topping up a sample
all(samp %in% sps(x, 7, prn = u))
#> [1] TRUE

#---- Other order-sampling methods ----
# Generate new order-sampling functions from the parameters of
# the inverse generalized Pareto distribution
igpd <- function(shape, scale = 1, location = 0) {
  if (shape == 0) {
    function(x) -scale * log(1 - x) + location
  } else {
    function(x) scale * (1 - (1 - x)^shape) / shape + location
  }
}

order_sampling2 <- function(x) order_sampling(igpd(x))

order_sampling2(1)(x, 6, prn = u) # sequential Poisson
#> [1]  3  6  7  8  9 11
order_sampling2(0)(x, 6, prn = u) # successive
#> [1]  3  6  7  8  9 11
order_sampling2(-1)(x, 6, prn = u) # Pareto
#> [1]  3  6  7  8  9 11
```
