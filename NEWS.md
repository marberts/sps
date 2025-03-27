## sps 0.6.0

- `inclusion_prob()` is now faster with a single stratum, improving the performance
of `sps()` and `expected_coverage()`.

- By default, `sps_repweights()` automatically picks the smallest
`tau` to keep the replicate weights from being negative. Inspired by
`svrep::make_gen_boot_factors()`.

- Now requires R >= 4.1, which has been the case for a while (closing #2).

## sps 0.5.4

- Added `becomes_ta()` to determine the sample size when a unit enter the
take-all stratum.

- Internal changes to the way classes are instantiated. No user-visible changes.

- Updated maintainer email.

## sps 0.5.3

- Documentation only; no changes to any functions.

## sps 0.5.2

- Added a vignette.

- A single sample size is now recycled for each stratum when drawing samples or calculating inclusion probabilities.

- Added a `cutoff` argument that puts units with sizes above the cutoff into a take-all stratum.

- `sps` objects now inherit from numeric instead of integer.

- Replacement methods for `length` and `levels` no longer mangle `sps` objects.

## sps 0.5.0

- `prop_allocation()` gets a new argument for breaking ties. The default now breaks ties according to the references; the old behavior can be had by setting `ties = "first"`.

- Argument names for `prop_allocation()`, `expected_coverage()`, and `sps_repweights()` have changed to be either more descriptive, or consistent with the names for other functions.

- Fixed a bug when calculating inclusion probabilities that could result in ties not breaking according to position (as documented) when `alpha > 0`.

- Simplified the codebase.

## sps 0.4.1

- Added a new argument `alpha` for calculating inclusion probabilities. It can be used to place units with inclusion probabilities close to 1 into the take-all stratum. This was implicitly 0 in previous versions, but the current default is 1e-4.

- All functions can now accept size vectors with zeros.

- `sps()` is now faster, especially for large populations.

- Added a function factory to generate other order sampling methods.

## sps 0.3.0

This version has a number of non-backwards compatible changes to address undesirable behavior with some functions.

- Largest-remainder rounding could result in `prop_allocation()` giving visibly non-proportional allocations, so it has been removed. The default is now the Jefferson/D'Hondt method.

- The `method` argument in `prop_allocation()` has been replaced with the `divisor` argument to supply a divisor *function* for rounding.

- The `initial` argument in `prop_allocation()` is recycled to ensure the initial allocation is feasible when supplying a single value. This change is not strictly backwards compatible as values that could not be ordinarily recycled no longer give an error.

- Supplying a vector of permanent random numbers to `sps()` or `ps()` that are generated with a given seed now gives the same result when setting that seed prior to calling `sps()` or `ps()`. This means that setting the seed to a given value can give a different sample compared to older versions, although permanent random numbers should be used for reproducible samples.

- The argument name for specifying strata is now `strata` instead of `s` in all functions. Partial matching means this change won't break existing code.

## sps 0.2.0

- `prop_allocation()` gains two new arguments:

    1. `initial` sets the initial allocation for each stratum. This saves from having to manually keep track of an initial allocation and adjust the sample size passed to `prop_allocation()`.

    2. `method` selects the apportionment method used to round a proportional allocation to integer values. In particular, highest-averages methods can be used in place of the largest-remainder method.

    The defaults for these arguments do not change the behavior of the function from previous versions.
    
- A few convenience functions have been added:

    1. `ps()` for drawing ordinary Poisson samples with the same interface as `sps()`.
    
    2. `inclusion_prob()` for generating inclusion probabilities for a frame.
    
    3. `expected_coverage()` for calculating the expected number of strata when sampling from a frame.
    
- Some of the internals have been updated to improve performance with large frames.

- Most functions do a little more argument checking. In particular, it is no longer possible to pass length 0 arguments to `sps()`, `prop_allocation()`, or `sps_repweights()`.

## sps 0.1.3

- Added the option to draw samples with permanent random numbers.

- Added methods for Math, Ops, [<-, and [[<- that strip attributes from `sps` objects so as not to treat the result as a sample.

## sps 0.1.2

- `sps()` now returns an integer vector (when possible), rather than a double.

- printing the matrix of replicate weights now shows row names (if any).
