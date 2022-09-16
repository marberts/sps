## Changes in version 0.1.4

- `prop_allocation()` gains two new arguments:

    1. `min` sets the minimum allocation for each stratum. This saves from having to manually keep track of an initial allocation and adjusting the sample size passed to `prop_allocation()`.

    2. `method` selects the apportionment method used to round a proportional allocation to integer values. In particular, highest-averages methods can be used in place of the largest-remainder method.

    The defaults for these arguments do not change the behavior of the function from previous versions.

## Changes in version 0.1.3

- Added the option to draw samples with permanent random numbers.

- Added methods for Math, Ops, [<-, and [[<- that strip attributes from `sps` objects so as not to treat the result as a sample.

## Changes in version 0.1.2

- `sps()` now returns an integer vector (when possible), rather than a double.

- printing the matrix of replicate weights now shows row names (if any).
