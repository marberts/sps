set.seed(4321)

stopifnot(
  exprs = {
    allocate(1:5, 0) == 0
    identical(allocate(1:3, 0, 1:3), c("1" = 0, "2" = 0, "3" = 0))
    allocate(1:5, 5) == 5
    identical(allocate(rep(1, 10), 4, rep(letters[1], 10)), c(a = 4))
    identical(allocate(rep(1, 10), 4, rep(letters[1:2], 5)), c(a = 2, b = 2))
    allocate(c(1, 1, 1, 1, 1, 1, 1, 10, 10, 10), 5, rep(letters[1:2], c(7, 3))) == 2:3
    identical(allocate(c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5, rep(letters[1:3], c(6, 2, 2))),
              c(a = 1, b = 2, c = 2))
    replicate(50, sum(allocate(rlnorm(50), 10, sample(letters, 50, TRUE))) == 10)
    replicate(100, sum(allocate(rlnorm(100), 40, sample(letters, 100, TRUE))) == 40)
    replicate(100, sum(allocate(rlnorm(1000), 100, sample(letters, 1000, TRUE))) == 100)
  },
  local = getNamespace("sps")
)
