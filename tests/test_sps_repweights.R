library(sps)

set.seed(1234)

unclass(sps_repweights(weights(sps(1:10, 0)), 10))
unclass(sps_repweights(weights(sps(1:10, 10)), 5, tau = 3))
unclass(sps_repweights(weights(sps(1:10, 10)), 10, dist = rnorm))
all(sps_repweights(1:5, tau = 2) > 0)
all(sps_repweights(1:5, tau = 2)[1, ] == 1)

try(sps_repweights(1:5, B = 0.9))
try(sps_repweights(1:5, tau = NA))

rownames(sps_repweights(structure(1:5, names = letters[1:5]), 5))
