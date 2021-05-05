set.seed(4321)

stopifnot(
  exprs = {
    allocate(1, 0) == 0
    allocate(rep(1, 10), 4, rep(letters[1], 10)) == 4
    allocate(rep(1, 10), 4, rep(letters[1:2], 5)) == c(2, 2)
    allocate(c(1, 1, 1, 1, 1, 1, 1, 10, 10, 10), 5, rep(letters[1:2], c(7, 3))) == c(2, 3)
    replicate(50, sum(allocate(rlnorm(50), 10, sample(letters, 50, TRUE))) == 10)
    replicate(100, sum(allocate(rlnorm(100), 40, sample(letters, 100, TRUE))) == 40)
    replicate(100, sum(allocate(rlnorm(1000), 100, sample(letters, 1000, TRUE))) == 100)
  },
  local = getNamespace("sps")
)
