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
all.equal(
  inclusion_prob(numeric(0), c(0, 0), factor(integer(0), 1:2)),
  numeric(0)
)
all.equal(
  inclusion_prob(rep(1, 6), c(2, 1), c(1, 1, 2, 1, 2, 2)),
  c(2, 2, 1, 2, 1, 1) / 3
)
all.equal(
  inclusion_prob(c(0, 1, 1, 1 + .Machine$double.eps), 3),
  c(0, 1, 1, 1)
)
all.equal(
  inclusion_prob(c(0, 1, 1, 1 - .Machine$double.eps), 3),
  c(0, 1, 1, 1)
)

# No rounds
x <- c(0:4, 10:8, 5:7, 0)
all.equal(inclusion_prob(x, 4), x / 55 * 4)

# One round
x <- c(x, 100)
all.equal(inclusion_prob(x, 4), c(x[1:12] / 55 * 3, 1))

# Two rounds
x <- c(20, x)
all.equal(inclusion_prob(x, 5), c(1, x[2:13] / 55 * 3, 1))

# Strata should be independent
all.equal(
  inclusion_prob(x, c(4, 3), gl(2, 7)),
  c(inclusion_prob(x[1:7], 4), inclusion_prob(x[8:14], 3))
)

# Compare with example 1 for sampling::inclusionprobabilities()
all.equal(
  inclusion_prob(1:20, 12),
  c(1:16 / 136 * 8, rep(1, 4))
)

all.equal(
  inclusion_prob(0:20, 12),
  c(0:16 / 136 * 8, rep(1, 4))
)

# Should agree with design weights
samp <- sps(x, c(4, 3), gl(2, 7))
all.equal(
  1 / inclusion_prob(x, c(4, 3), gl(2, 7))[samp],
  weights(samp)
)

# Add more TAs with alpha
x <- c(0, 4, 1, 4, 5)

all.equal(
  inclusion_prob(x, 3, alpha = 0.1),
  c(x[-5] / 9 * 2, 1)
)

all.equal(
  inclusion_prob(x, 3, alpha = 0.2),
  c(x[-(4:5)] / 5, 1, 1)
)

all.equal(
  inclusion_prob(x, 3, alpha = 0.3),
  c(0, 1, 0, 1, 1)
)
