library(sps)

set.seed(4321)

prop_allocation(1:5, 0)
prop_allocation(1:3, 0, 1:3)
prop_allocation(1:5, 5)
prop_allocation(rep(1, 10), 4, rep(letters[1], 10))
prop_allocation(rep(1, 10), 4, rep(letters[1:2], 5))
prop_allocation(c(1, 1, 1, 1, 1, 1, 1, 10, 10, 10), 5, rep(letters[1:2], c(7, 3)))
prop_allocation(c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5, rep(letters[1:3], c(6, 2, 2)))
all(replicate(50, sum(prop_allocation(rlnorm(50), 10, sample(letters, 50, TRUE))) == 10))

try(prop_allocation(c(-1, 1), 5))
try(prop_allocation(1:5, -1))
try(prop_allocation(1:4, 5))