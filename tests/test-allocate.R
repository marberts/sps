library(sps)

set.seed(4321)

# Corner cases
all.equal(
  prop_allocation(0, 0),
  c("1" = 0)
)
all.equal(
  prop_allocation(1:3, 0, 1:3),
  c("1" = 0, "2" = 0, "3" = 0)
)
all.equal(
  prop_allocation(1:5, 5),
  c("1" = 5)
)
all.equal(
  prop_allocation(rep(1, 10), 4, factor(rep(letters[1], 10), levels = c("a", "b"))),
  c(a = 4, b = 0)
)
all.equal(
  prop_allocation(1:4, 2, initial = 3),
  c("1" = 2)
)

# Simple allocations
all.equal(
  prop_allocation(rep(1, 10), 4, rep(letters[1:2], 5)),
  c(a = 2, b = 2)
)
all.equal(
  prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5)),
  c(a = 0, b = 4)
)
all.equal(
  prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5), initial = 1),
  c(a = 1, b = 3)
)
all.equal(
  prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5), initial = 2),
  c(a = 2, b = 2)
)
all.equal(
  prop_allocation(c(rep(10, 8), 1, 1), 5, c(rep("a", 8), "b", "b"), initial = 3),
  c(a = 3, b = 2)
)
all.equal(
  prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5), initial = c(3, 1)),
  c(a = 3, b = 1)
)
all.equal(
  prop_allocation(rep(c(1, 10), 5), 4, factor(rep(letters[1:2], 5), levels = letters[1:3]), initial = c(2, 1, 0)),
  c(a = 2, b = 2, c = 0)
)
all.equal(
  prop_allocation(rep(1, 100), 10, rep(1:4, c(10, 20, 30, 40))),
  c("1" = 1, "2" = 2, "3" = 3, "4" = 4)
)
all.equal(
  prop_allocation(c(0, 0, 100, 1, 1, 1), 3, gl(2, 3)),
  c("1" = 1, "2" = 2)
)

# Multiple iterations
all.equal(
  prop_allocation(c(1, 1, 1, 1, 1, 1, 1, 10, 10, 10), 5, rep(letters[1:2], c(7, 3))),
  c(a = 2, b = 3)
)
all.equal(
  prop_allocation(c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5, rep(letters[1:3], c(6, 2, 2))),
  c(a = 1, b = 2, c = 2)
)
all.equal(
  prop_allocation(c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5, rep(letters[1:3], c(6, 2, 2)), divisor = identity),
  c(a = 1, b = 2, c = 2)
)

# Allocation for each stratum should add up to total sample size
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE))) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), divisor = function(x) x + 0.5)) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), divisor = function(x) x + 2)) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), divisor = function(x) sqrt(x * (x + 1)))) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), divisor = function(x) x + 1 / 3)) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), divisor = identity)) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), divisor = function(x) x * (x + 1) / (x + 0.5))) == 10))

# Alabama paradox
all.equal(
  prop_allocation(rep(1, 14), 10, rep(1:3, c(6, 6, 2))),
  c("1" = 5, "2" = 4, "3" = 1)
)
all.equal(
  prop_allocation(rep(1, 14), 11, rep(1:3, c(6, 6, 2))),
  c("1" = 5, "2" = 5, "3" = 1)
)

# example from https://en.wikipedia.org/wiki/Highest_averages_method
x <- rep(1, 1e5)
s <- as.factor(rep(1:6, c(47000, 16000, 15900, 12000, 6000, 3100)))

all.equal(
  prop_allocation(x, 10, s),
  c("1" = 5, "2" = 2, "3" = 2, "4" = 1, "5" = 0, "6" = 0)
)

all.equal(
  prop_allocation(x, 10, s, divisor = function(a) a + 0.5),
  c("1" = 4, "2" = 2, "3" = 2, "4" = 1, "5" = 1, "6" = 0)
)

all.equal(
  prop_allocation(x, 10, s, divisor = function(a) sqrt(a * (a + 1))),
  c("1" = 4, "2" = 2, "3" = 1, "4" = 1, "5" = 1, "6" = 1)
)

all.equal(
  prop_allocation(x, 10, s, divisor = function(a) a),
  c("1" = 3, "2" = 2, "3" = 2, "4" = 1, "5" = 1, "6" = 1)
)

# Checks for error messages
try(prop_allocation(c(-1, 1), 5))
try(prop_allocation(1:5, -1))
try(prop_allocation(1:4, 5))
try(prop_allocation(1:4, 2, initial = c(3, 0)))
try(prop_allocation(1:4, 2, gl(2, 2), initial = c(3, 0)))
try(prop_allocation(1:4, 2, gl(2, 2), initial = c(2, 2)))
try(prop_allocation(1:4, 0, factor(1:4, levels = integer(0))))

# Test coverage
all.equal(expected_coverage(1:6, 6), 1)
all.equal(expected_coverage(1:6, 0), 0)
all.equal(expected_coverage(1:6, 3, 1:6), 3)

all.equal(
  expected_coverage(1:10, 4, gl(2, 5)),
  expected_coverage(1:10, 4, gl(2, 5, labels = 1:3))
)
