library(sps)

set.seed(123454)

# Corner cases
all.equal(
  unclass(sps(1:5, 0)),
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
  unclass(ps(1:5, 0)),
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

# Two rounds of TA removal
samp <- sps(c(20, 1:10, 100), 5)
all(samp[c(1, 5)] == c(1, 12))
all(levels(samp) == c("TA", rep("TS", 3), "TA"))
all(weights(samp)[c(1, 5)] == 1)
all(weights(samp)[-c(1, 5)] > 1)

samp <- ps(c(20, 1:10, 100), 5)
last <- length(samp)
all(samp[c(1, last)] == c(1, 12))
all(levels(samp) == c("TA", rep("TS", last - 2), "TA"))
all(weights(samp)[c(1, last)] == 1)
all(weights(samp)[-c(1, last)] > 1)

# Return value should be an integer
is.integer(sps(1:5, 3))
is.integer(sps(1:5, 0))
is.integer(ps(1:5, 3))
is.integer(ps(1:5, 0))

# Strata sizes should add up
s <- factor(sample(letters, 100, TRUE), letters)
x <- rlnorm(100)
alloc <- prop_allocation(x, 50, s)
samp <- sps(x, alloc, s)
all.equal(
  tabulate(factor(s[samp], letters)), 
  as.vector(alloc)
)

# Tests for permanent random numbers
set.seed(4321)
prn <- runif(10)
all.equal(
  sps(c(1:9, 100), 5, prn = prn), 
  sps(c(1:9, 100), 5, prn = prn)
)
set.seed(4321)
all.equal(
  sps(1:10, 5, prn = prn), 
  sps(1:10, 5)
)
set.seed(4321)
all.equal(
  ps(c(1:9, 100), 5, prn = prn), 
  ps(c(1:9, 100), 5)
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
all(order(weights(sps(1:10, 4))) == 4:1)

# Mathematical functions should treat 'sps' objects as numeric vectors
inherits(log(samp), "numeric") 
inherits(1L + samp, "integer")
inherits(samp / 2, "numeric")
inherits(samp > samp, "logical")
inherits(-samp, "integer")

# And replacement methods
samp[1] <- 1
inherits(samp, "numeric")

# Tests for error messages
try(sps(-1, 1))
try(sps(1, NA))
try(sps(1:4, 2, c(1, 1, 2, 2)))
try(sps(1:4, c(5, 2), c(1, 1, 2, 2)))
try(sps(1:4, 2, prn = c(0.1, 0.1, 0.1, NA)))
