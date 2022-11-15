## Changes in version 0.2.1

- The `initial` argument in `prop_allocation()` is recycled to ensure the initial allocation is feasible when supplying a single value.

- Supplying a vector of permanent random numbers to `sps()` or `ps()` that are generated with a given seed now gives the same result when setting that seed prior to calling `sps()` or `ps()`. This means that setting the seed to a given value can give a different sample compared to older versions, although permanent random numbers should be used for reproducible samples.

## Changes in version 0.2.0

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

## Changes in version 0.1.3

- Added the option to draw samples with permanent random numbers.

- Added methods for Math, Ops, [<-, and [[<- that strip attributes from `sps` objects so as not to treat the result as a sample.

## Changes in version 0.1.2

- `sps()` now returns an integer vector (when possible), rather than a double.

- printing the matrix of replicate weights now shows row names (if any).
