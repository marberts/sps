library(sps)

set.seed(123454)

# Corner cases
all.equal(
  unclass(sps(1:10, 0)),
  structure(integer(0), weights = numeric(0), levels = character(0))
)
all.equal(
  unclass(sps(1:10, 10)),
  structure(1:10, weights = rep(1, 10), levels = rep("TA", 10))
)
all.equal(
  unclass(sps(1:10, c(5, 0), gl(2, 5))),
  structure(1:5, weights = rep(1, 5), levels = rep("TA", 5))
)
all.equal(
  unclass(ps(1:10, 0)),
  structure(integer(0), weights = numeric(0), levels = character(0))
)
all.equal(
  unclass(ps(1:10, 10)),
  structure(1:10, weights = rep(1, 10), levels = rep("TA", 10))
)
all.equal(
  unclass(ps(1:10, c(5, 0), gl(2, 5))),
  structure(1:5, weights = rep(1, 5), levels = rep("TA", 5))
)

# Result should be sorted
x <- c(20, 1:10, 100, 0, 0)
samp <- sps(x, c(3, 2, 2), c(1, 1, 2, 1, 3, 1, 2, 3, 2, 1, 3, 3, 3, 1))
all.equal(
  as.integer(samp),
  sort(samp)
)

# Two rounds of TA removal
samp <- sps(x, 5)
all(samp[c(1, 5)] == c(1, 12))
all(levels(samp) == c("TA", rep("TS", 3), "TA"))
all(weights(samp)[c(1, 5)] == 1)
all(weights(samp)[-c(1, 5)] > 1)

samp <- ps(x, 5)
last <- length(samp)
all(samp[c(1, last)] == c(1, 12))
all(levels(samp) == c("TA", rep("TS", last - 2), "TA"))
all(weights(samp)[c(1, last)] == 1)
all(weights(samp)[-c(1, last)] > 1)

# Use alpha to make all units TAs
all.equal(
  levels(sps(c(0:5, 0:5), c(3, 3), rep(1:2, each = 6), alpha = c(0.51, 0))),
  c(rep("TA", 3), "TS", "TS", "TA")
)

# Does noting when units are already TAs
all.equal(
  sps(0:5, 5),
  sps(0:5, 5, alpha = 0.9)
)

# Return value should be an integer
is.integer(sps(1:5, 3))
is.integer(sps(1:5, 0))
is.integer(ps(1:5, 3))
is.integer(ps(1:5, 0))

# Strata sizes should add up
s <- factor(sample(letters[-1], 100, TRUE), letters)
x <- rlnorm(100)
alloc <- prop_allocation(x, 50, s)
samp <- sps(x, alloc, s)
all.equal(
  tabulate(s[samp], nbins = 26), 
  as.vector(alloc)
)

# Tests for permanent random numbers
set.seed(4321)
prn <- runif(11)
all.equal(
  sps(c(100, 1:9, 100), 5, prn = prn), 
  sps(c(100, 1:9, 100), 5, prn = prn)
)
set.seed(4321)
all.equal(
  sps(c(100, 1:9, 100), 5, prn = prn), 
  sps(c(100, 1:9, 100), 5)
)
set.seed(4321)
all.equal(
  ps(c(100, 1:9, 100), 5, prn = prn), 
  ps(c(100, 1:9, 100), 5)
)

# Test for extending a stratified sample
set.seed(1432)
u <- runif(100)
x <- c(runif(98), 100, 200)
samp <- sps(x, c(5, 6), rep(1:2, each = 50), u)
drop <- c(10, 100, 54)
samp2 <- sps(x[-drop], c(4, 4), rep(1:2, each = 50)[-drop], u[-drop])
all.equal(
  x[samp[-match(drop, samp)]], 
  x[-drop][samp2]
)

# Weights should be monotonic
all(order(weights(sps(0:10, 4))) == 4:1)

# Mathematical functions should treat 'sps' objects as numeric vectors
inherits(log(samp), "numeric") 
inherits(1L + samp, "integer")
inherits(samp / 2, "numeric")
inherits(samp > samp, "logical")
inherits(-samp, "integer")

# And replacement methods
samp[1] <- 1
inherits(samp, "numeric")

# Other order sampling
pareto <- order_sampling(function(x) x / (1 - x))

u <- runif(20)
all.equal(
  as.integer(pareto(rep(1, 20), c(5, 6), rep(1:2, 10), u)),
  sort(c(seq(1, 20, 2)[order(u[seq(1, 20, 2)])[1:5]], seq(2, 20, 2)[order(u[seq(2, 20, 2)])[1:6]]))
)

# Shift prns
u <- 1:9 / 10
v <- (u - 0.5) %% 1

all.equal(
  as.integer(pareto(rep(1, 9), 5, prn = u)),
  1:5
)

all.equal(
  as.integer(pareto(rep(1, 9), 5, prn = v)),
  5:9
)
