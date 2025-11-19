# Iteratively draw a sequential Poisson sample

Create a function that draws new units, one at a time, according to the
sequential Poisson method without replacing previously sampled units.

## Usage

``` r
sps_iterator(x, n = 0L, prn = NULL, alpha = 0.001, cutoff = Inf)
```

## Arguments

- x:

  A positive and finite numeric vector of sizes for units in the
  population (e.g., revenue for drawing a sample of businesses).

- n:

  A positive integer giving the initial sample size for the iterator.

- prn:

  A numeric vector of permanent random numbers for units in the
  population, distributed uniform between 0 and 1. The default does not
  use permanent random numbers, instead generating a random vector when
  the function is called.

- alpha:

  A number between 0 and 1. Units with inclusion probabilities greater
  than or equal to 1 - `alpha` are set to 1. The default is slightly
  larger than 0.

- cutoff:

  A numeric cutoff. Units with `x >= cutoff` get an inclusion
  probability of 1. The default does not apply a cutoff.

## Value

A function that returns the next unit in the sample. It take a single
argument giving the sentinel value to indicate that there are no units
left to sample (default `NULL`).

## Examples

``` r
prn <- runif(5)
s <- sps_iterator(1:5, prn = prn)
s()
#> [1] 5
s()
#> [1] 2
s()
#> [1] 3

# Same as drawing the sample with 3 units.
sps(1:5, 3, prn = prn)
#> [1] 2 3 5
```
