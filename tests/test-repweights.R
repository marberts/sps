library(sps)

set.seed(1234)

# Corner cases
all.equal(
  unclass(sps_repweights(weights(sps(1:10, 10)), 5, tau = 3)),
  structure(matrix(1, 10, 5), tau = 3)
)
all.equal(
  unclass(sps_repweights(weights(sps(1:10, 10)), 10, dist = rnorm)),
  structure(matrix(1, 10, 10), tau = 1)
)

# All rep weights should be positive with the TA strata having all 1s
all(sps_repweights(1:5, tau = 2) > 0)
all(sps_repweights(1:5, tau = 2)[1, ] == 1)

# Check error messages
try(sps_repweights(1:5, B = 0.9))
try(sps_repweights(1:5, tau = NA))

# Printing should preserve names
sps_repweights(structure(rep(1, 5), names = letters[1:5]), 5)
