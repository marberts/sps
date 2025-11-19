# Construct a proportional allocation

Generate a proportional-to-size allocation for stratified sampling.

## Usage

``` r
prop_allocation(
  x,
  n,
  strata,
  initial = 0L,
  divisor = divisor_method("Jefferson/D'Hondt"),
  ties = c("largest", "first")
)

divisor_method(
  name = c(
    "Jefferson/D'Hondt",
    "Webster/Sainte-Lague",
    "Imperiali",
    "Huntington-Hill",
    "Danish",
    "Adams",
    "Dean"
    )
  )
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

- initial:

  A positive integer vector giving the initial (or minimal) allocation
  for each stratum, ordered according to the levels of `strata`. A
  single integer is recycled for each stratum using a special algorithm
  to ensure a feasible allocation; see details. Non-integers are
  truncated towards 0. The default allows for no units to be allocated
  to a stratum.

- divisor:

  A function for the divisor (highest-averages) apportionment method.
  The default uses the Jefferson/D'Hondt method. See details for other
  possible functions.

- ties:

  Either 'largest' to break ties in favor of the stratum with the
  largest size (the default), or 'first' to break ties in favor of the
  ordering of `strata`.

- name:

  Name of the divisor function. See details.

## Value

`prop_allocation()` returns a named integer vector of sample sizes for
each stratum in `strata`.

`divisor_method()` returns a function giving the desired divisor
function.

## Details

The `prop_allocation()` function gives a sample size for each level in
`strata` that is proportional to the sum of `x` across strata and adds
up to `n`. This is done using the divisor (highest-averages)
apportionment method (Balinksi and Young, 1982, Appendix A), for which
there are a number of different divisor functions:

- Jefferson/D'Hondt:

  `\(a) a + 1`

- Webster/Sainte-Laguë:

  `\(a) a + 0.5`

- Imperiali:

  `\(a) a + 2`

- Huntington-Hill:

  `\(a) sqrt(a * (a + 1))`

- Danish:

  `\(a) a + 1 / 3`

- Adams:

  `\(a) a`

- Dean:

  `\(a) a * (a + 1) / (a + 0.5)`

Note that a divisor function with \\d(0) = 0\\ (i.e., Huntington-Hill,
Adams, Dean) should have an initial allocation of at least 1 for all
strata. In all cases, ties are broken according to the sum of `x` if
`ties = 'largest'`; otherwise, if `ties = 'first'`, then ties are broken
according to the levels of `strata`.

In cases where the number of units with non-zero size in a stratum is
smaller than its allocation, the allocation for that stratum is set to
the number of available units, with the remaining sample size
reallocated to other strata proportional to `x`. This is similar to
`PROC SURVEYSELECT` in SAS with `ALLOC = PROPORTIONAL`.

Passing a single integer for the initial allocation first checks that
recycling this value for each stratum does not result in an allocation
larger than the sample size. If it does, then the value is reduced so
that recycling does not exceed the sample size. This recycled vector can
be further reduced in cases where it exceeds the number of units in a
stratum, the result of which is the initial allocation. This special
recycling ensures that the initial allocation is feasible.

## References

Balinksi, M. L. and Young, H. P. (1982). *Fair Representation: Meeting
the Ideal of One Man, One Vote*. Yale University Press.

## See also

[`sps()`](https://marberts.github.io/sps/reference/sps.md) for
stratified sequential Poisson sampling.

[`expected_coverage()`](https://marberts.github.io/sps/reference/expected_coverage.md)
to calculate the expected number of strata in a sample without
stratification.

`strAlloc()` in the PracTools package and the optimall package for other
allocation methods.

## Examples

``` r
# Make a population with units of different size
x <- c(rep(1:9, each = 3), 100, 100, 100)

# ... and 10 strata
s <- rep(letters[1:10], each = 3)

# Generate an allocation
prop_allocation(x, 15, s, initial = 1)
#> a b c d e f g h i j 
#> 1 1 1 1 1 1 2 2 2 3 
```
