library(sps)

set.seed(4321)

allocate(1:5, 0)
allocate(1:3, 0, 1:3)
allocate(1:5, 5)
allocate(rep(1, 10), 4, rep(letters[1], 10))
allocate(rep(1, 10), 4, rep(letters[1:2], 5))
allocate(c(1, 1, 1, 1, 1, 1, 1, 10, 10, 10), 5, rep(letters[1:2], c(7, 3)))
allocate(c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5, rep(letters[1:3], c(6, 2, 2)))
all(replicate(50, sum(allocate(rlnorm(50), 10, sample(letters, 50, TRUE))) == 10))

try(allocate(c(-1, 1), 5))
try(allocate(1:5, -1))
try(allocate(1:4, 5))