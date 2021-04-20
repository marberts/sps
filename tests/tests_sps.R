set.seed(1234)

stopifnot(
  exprs = {
    identical(sps(1, 0), 
              structure(numeric(0), weights = numeric(0), levels = character(0), class = c("sps", "numeric")))
    identical(sps(integer(0), integer(0)), 
              structure(numeric(0), weights = numeric(0), levels = character(0), class = c("sps", "numeric")))
    sps(1, 1) == 1
    setequal(sps(1:10, 10), 1:10)
    weights(sps(1:10, 10)) == 1
    levels(sps(1:10, 10)) == "TA"
    sps(c(1:10, 20, 100), 5)[1:2] == 12:11
    levels(sps(c(1:10, 20, 100), 5)) == c("TA", "TA", "TS", "TS", "TS")
    sps(c(1:10, 90, 100), 5)[1:2] == 11:12
    levels(sps(c(1:10, 90, 100), 5)) == c("TA", "TA", "TS", "TS", "TS")
    weights(sps(1:10, 1)) >= 5.5
    replicate(100, all(weights(sps(rlnorm(100), 20)) >= 1))
    replicate(100, all(sps(rlnorm(100), 20) %in% 1:100))
    replicate(100, all(levels(sps(rlnorm(100), 20)) %in% c("TS", "TA")))
    replicate(100, length(unique(sps(rlnorm(100), 20)))) == 20
    replicate(100, length(weights(sps(rlnorm(100), 20)))) == 20
    replicate(100, length(levels(sps(rlnorm(100), 20)))) == 20
    sps(1:4, c(1, 0), c(1, 1, 2, 2)) == 2
    sps(1:4, c(1, 2), c(1, 1, 2, 2)) == c(2, 4, 3)
  },
  local = getNamespace("sps")
)
