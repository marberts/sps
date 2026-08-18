# Performance

The sequential Poisson method, and order sampling methods more
generally, are simple and consequently not computationally expensive.
This makes them suitable for a range of different applications,
especially when drawing a sample from a large population. Despite this,
there are two optimizations in this package to keep drawing samples
fast.

## Calculating inclusion probabilities

The first optimization concerns the inclusion probabilities when there
are take-all units. As seen in
[`vignette("take-all")`](https://marberts.github.io/sps/articles/take-all.md),
the algorithm finds take-all units one at a time without recomputing the
inclusion probabilities many times. This is much faster than the naive
approach when drawing a large sample, and is on par with the usual
algorithm that finds take-all units in batches.

``` r

library(sps)

# Make a population with 200 take-all units.
x <- c(rep(1, 1e6 - 200), rep(1e6, 200))
n <- 1e3


# Naive implementation.
ip <- function(x, n, alpha = 0.001) {
  p <- \(x, n) x * (n / sum(x))
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
    #> 1 inclusion_prob(x, n)                    50.81ms    72.5MB    10
    #> 2 ip(x, n)                                  1.04s       3GB     1
    #> 3 sampling::inclusionprobabilities(x, n)  49.29ms   133.5MB     8

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

options(sps.usekit = TRUE)
bench::mark(sps(x, n))[c("expression", "median", "mem_alloc", "n_itr")]
```

    #> # A tibble: 1 × 4
    #>   expression   median mem_alloc n_itr
    #>   <bch:expr> <bch:tm> <bch:byt> <int>
    #> 1 sps(x, n)    64.8ms     111MB     8

``` r

options(sps.usekit = FALSE)
bench::mark(sps(x, n))[c("expression", "median", "mem_alloc", "n_itr")]
```

    #> # A tibble: 1 × 4
    #>   expression   median mem_alloc n_itr
    #>   <bch:expr> <bch:tm> <bch:byt> <int>
    #> 1 sps(x, n)     155ms     126MB     4

Partial sorting generally speeds up drawing smaller samples as well, but
the effect is not as large because the sequential Poisson method is
already quick.
