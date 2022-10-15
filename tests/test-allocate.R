library(sps)

set.seed(4321)

# Corner cases
all.equal(
  prop_allocation(1:5, 0),
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
  prop_allocation(rep(1, 10), 4, rep(letters[1], 10)),
  c(a = 4)
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
  prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5), min = 1),
  c(a = 1, b = 3)
)
all.equal(
  prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5), min = 2),
  c(a = 2, b = 2)
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
  prop_allocation(c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5, rep(letters[1:3], c(6, 2, 2)), method = "Adams"),
  c(a = 1, b = 2, c = 2)
)

# Allocation for each stratum should add up to total sample size
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE))) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), method = "D'Hondt")) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), method = "Webster")) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), method = "Imperiali")) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), method = "Huntington-Hill")) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), method = "Danish")) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), method = "Adams")) == 10))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE), method = "Dean")) == 10))

# Alabama paradox
all.equal(
  prop_allocation(rep(1, 14), 10, rep(1:3, c(6, 6, 2))),
  c("1" = 4, "2" = 4, "3" = 2)
)
all.equal(
  prop_allocation(rep(1, 14), 11, rep(1:3, c(6, 6, 2))),
  c("1" = 5, "2" = 5, "3" = 1)
)
all.equal(
  prop_allocation(rep(1, 14), 10, rep(1:3, c(6, 6, 2)), method = "D'Hondt"),
  c("1" = 5, "2" = 4, "3" = 1)
)
all.equal(
  prop_allocation(rep(1, 14), 11, rep(1:3, c(6, 6, 2)), method = "D'Hondt"),
  c("1" = 5, "2" = 5, "3" = 1)
)

# Checks for error messages
try(prop_allocation(c(-1, 1), 5))
try(prop_allocation(1:5, -1))
try(prop_allocation(1:4, 5))
try(prop_allocation(1:4, 2, min = 3))
try(prop_allocation(1:4, 0, factor(1:4, levels = integer(0))))

# Test coverage
all.equal(expected_coverage(1:6, 6), 1)
all.equal(expected_coverage(1:6, 0), 0)
all.equal(expected_coverage(1:6, 3, 1:6), 3)

all.equal(
  expected_coverage(1:10, 4, gl(2, 5)),
  expected_coverage(1:10, 4, gl(2, 5, labels = 1:3))
)
