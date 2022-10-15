library(sps)

set.seed(14235)

# Corner cases
all.equal(
  inclusion_prob(1:3, c(0, 1, 0), factor(c(2, 2, 2), levels = 1:3)),
  1:3 / 6
)
all.equal(
  inclusion_prob(1:6, c(0, 3), c(1, 1, 2, 1, 2, 2)),
  c(0, 0, 1, 0, 1, 1)
)

# No rounds
x <- c(1:4, 10:8, 5:7)
all.equal(inclusion_prob(x, 4), x / 55 * 4)

# One round
x <- c(x, 100)
all.equal(inclusion_prob(x, 4), c(x[1:10] / 55 * 3, 1))

# Two rounds
x <- c(20, x)
all.equal(inclusion_prob(x, 5), c(1, x[2:11] / 55 * 3, 1))

# Strata should be independent
all.equal(
  inclusion_prob(x, c(4, 3), gl(2, 6)),
  c(inclusion_prob(x[1:6], 4), inclusion_prob(x[7:12], 3))
)

# Compare with example 1 for sampling::inclusionprobabilities()
all.equal(
  inclusion_prob(1:20, 12),
  c(1:16 / 136 * 8, rep(1, 4))
)

# Should agree with design weights
samp <- sps(x, c(4, 3), gl(2, 6))
all.equal(
  1 / inclusion_prob(x, c(4, 3), gl(2, 6))[samp],
  weights(samp)
)
