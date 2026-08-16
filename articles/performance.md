# Performance

The sequential Poisson method, and order sampling methods more
generally, are simple and consequently not computationally expensive.
Despite this, there are two optimizations in this package to keep
drawing samples fast.

## Calculating inclusion probabilities

The first optimization concerns the inclusion probabilities when there
are take-all units. As seen in
[`vignette("take-all")`](https://marberts.github.io/sps/articles/take-all.md),
the algorithm finds take-all units one at a time without recomputing the
inclusion probabilities many times. This is much faster than the naive
approach when drawing a large sample and is on par with the usual
algorithm that finds take-all units in batches.

``` r

library(sps)

set.seed(85834)

x <- rlnorm(1e5)
n <- 1e4

p <- function(x, n) x / sum(x) * n

# How many take-all units?
sum(becomes_ta(x) <= n)
```

    #> [1] 240

``` r

# Naive implementation.
ip <- function(x, n, alpha = 0.001) {
  ta_units <- integer(0)
  pi <- p(x, n)
  max_ts <- which.max(pi)
  while (pi[max_ts] > 1 - alpha) {
    ta_units <- c(ta_units, max_ts)
    pi <- p(replace(pi, max_ts, 0), n - length(ta_units))
    max_ts <- which.max(pi)
  }
  replace(pi, ta_units, 1)
}

bench::mark(
  inclusion_prob(x, n),
  ip(x, n),
  sampling::inclusionprobabilities(x, n)
)[c("expression", "median", "mem_alloc", "n_itr")]
```

    #> # A tibble: 3 × 4
    #>   expression                               median mem_alloc n_itr
    #>   <bch:expr>                             <bch:tm> <bch:byt> <int>
    #> 1 inclusion_prob(x, n)                     6.13ms    7.67MB    78
    #> 2 ip(x, n)                               124.48ms  367.88MB     5
    #> 3 sampling::inclusionprobabilities(x, n)   4.66ms   19.45MB    80

## Partial sorting

The second optimization is recognizing that both the computation of
inclusion probabilities and the sequential Poisson method can benefit
from partial sorting algorithms. In both cases, only the \\n\\
largest/smallest elements of a vector are needed and, when installed,
the `topn()` function from the [kit](https://fastverse.org/kit/) package
is used to avoid a complete sort. This is a drop-in replacement for
`order(x)[1:n]` that can be faster when `n` is much smaller than the
length of `x`, and can have a modest impact on performance when drawing
a sample from a large population.

``` r

x <- rlnorm(1e6)
n <- 1e3

options(sps.usekit = TRUE)
bench::mark(sps(x, n))[c("expression", "median", "mem_alloc", "n_itr")]
```

    #> # A tibble: 1 × 4
    #>   expression   median mem_alloc n_itr
    #>   <bch:expr> <bch:tm> <bch:byt> <int>
    #> 1 sps(x, n)    59.9ms    76.3MB     7

``` r

options(sps.usekit = FALSE)
bench::mark(sps(x, n))[c("expression", "median", "mem_alloc", "n_itr")]
```

    #> # A tibble: 1 × 4
    #>   expression   median mem_alloc n_itr
    #>   <bch:expr> <bch:tm> <bch:byt> <int>
    #> 1 sps(x, n)    88.1ms    80.1MB     5

Partial sorting generally speeds up drawing smaller samples as well, but
the effect is not as large because the sequential Poisson method is
already quick.
