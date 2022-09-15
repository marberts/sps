library(sps)

set.seed(4321)

# Corner cases
prop_allocation(1:5, 0)
prop_allocation(1:3, 0, 1:3)
prop_allocation(1:5, 5)
prop_allocation(rep(1, 10), 4, rep(letters[1], 10))

# Simple allocations
prop_allocation(rep(1, 10), 4, rep(letters[1:2], 5))
prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5))
prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5), min = 1)
prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5), min = 2)

# Multiple iterations
prop_allocation(c(1, 1, 1, 1, 1, 1, 1, 10, 10, 10), 5, rep(letters[1:2], c(7, 3)))
prop_allocation(c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5, rep(letters[1:3], c(6, 2, 2)))
prop_allocation(c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5, rep(letters[1:3], c(6, 2, 2)), method = "Adams")

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
prop_allocation(rep(1, 14), 10, rep(1:3, c(6, 6, 2)))
prop_allocation(rep(1, 14), 11, rep(1:3, c(6, 6, 2)))

prop_allocation(rep(1, 14), 10, rep(1:3, c(6, 6, 2)), method = "D'Hondt")
prop_allocation(rep(1, 14), 11, rep(1:3, c(6, 6, 2)), method = "D'Hondt")

# Checks for error messages
try(prop_allocation(c(-1, 1), 5))
try(prop_allocation(1:5, -1))
try(prop_allocation(1:4, 5))
try(prop_allocation(1:4, 2, min = 3))
