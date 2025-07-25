set.seed(123454)

test_that("corner cases work as expected", {
  expect_identical(
    unclass(sps(numeric(0), 0)),
    structure(integer(0), weights = numeric(0))
  )
  expect_identical(
    unclass(sps(0, 0)),
    structure(integer(0), weights = numeric(0))
  )
  expect_identical(
    unclass(sps(1:10, 0)),
    structure(integer(0), weights = numeric(0))
  )
  expect_identical(
    unclass(sps(1:10, 10)),
    structure(1:10, weights = rep(1, 10))
  )
  expect_identical(
    unclass(sps(1:10, c(5, 0), gl(2, 5))),
    structure(1:5, weights = rep(1, 5))
  )
  expect_identical(
    unclass(ps(numeric(0), 0)),
    structure(integer(0), weights = numeric(0))
  )
  expect_identical(
    unclass(ps(0, 0)),
    structure(integer(0), weights = numeric(0))
  )
  expect_identical(
    unclass(ps(1:10, 0)),
    structure(integer(0), weights = numeric(0))
  )
  expect_identical(
    unclass(ps(1:10, 10)),
    structure(1:10, weights = rep(1, 10))
  )
  expect_identical(
    unclass(ps(1:10, c(5, 0), gl(2, 5))),
    structure(1:5, weights = rep(1, 5))
  )
})

test_that("argument checking works", {
  expect_error(sps(-1:4, c(2, 2), gl(2, 3)))
  expect_error(sps(c(NA, 1:5), c(2, 2), gl(2, 3)))
  expect_error(sps(numeric(0), c(2, 2), gl(2, 3)))
  expect_error(sps(numeric(0), 0, factor(integer(0))))
  expect_error(sps(c(0, 0, 1:4), c(2, 2), gl(2, 3)))
  expect_error(sps(c(0, 0, 1:4), 5))
  expect_error(sps(1:6, c(-2, 2), gl(2, 3)))
  expect_error(sps(1:6, c(NA, 2), gl(2, 3)))
  expect_error(sps(1:6, integer(0), gl(2, 3)))
  expect_error(sps(1:6, c(2, 2)))
  expect_error(sps(1:6, c(2, 2), gl(2, 2)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3)[c(1:5, 7)]))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), alpha = c(0, 1.5)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), alpha = c(0, NA)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), alpha = c(0, 0, 0)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), alpha = integer(0)))
  expect_error(sps(1:6, 2, alpha = c(0, 0)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), prn = c(0, runif(4), 1)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), prn = 1:7 / 10))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), prn = -1:4 / 10))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), prn = c(NA, 1:5) / 10))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), prn = numeric(0)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), cutoff = 2))
})

test_that("results are sorted", {
  x <- c(20, 1:10, 100, 0, 0)
  samp <- sps(x, c(3, 2, 2), c(1, 1, 2, 1, 3, 1, 2, 3, 2, 1, 3, 3, 3, 1))
  expect_identical(
    as.integer(samp),
    sort(samp)
  )
  # weights should be monotonic
  expect_identical(
    order(weights(sps(0:10, 4))), 4:1
  )
})

test_that("two rounds of TA removal works", {
  x <- c(20, 1:10, 100, 0, 0)
  samp <- sps(x, 5)
  expect_equal(samp[c(1, 5)], c(1, 12))
  expect_equal(levels(samp), c("TA", rep("TS", 3), "TA"))
  expect_true(all(weights(samp)[c(1, 5)] == 1))
  expect_true(all(weights(samp)[-c(1, 5)] > 1))

  samp <- ps(x, 5)
  last <- length(samp)
  expect_equal(samp[c(1, last)], c(1, 12))
  expect_equal(levels(samp), c("TA", rep("TS", last - 2), "TA"))
  expect_true(all(weights(samp)[c(1, last)] == 1))
  expect_true(all(weights(samp)[-c(1, last)] > 1))
  # use alpha to make all units TAs
  expect_identical(
    levels(sps(c(0:5, 0:5), c(3, 3), rep(1:2, each = 6), alpha = c(0.51, 0))),
    c(rep("TA", 3), "TS", "TS", "TA")
  )
  # does nothing when units are already TAs
  expect_identical(
    sps(0:5, 5),
    sps(0:5, 5, alpha = 0.9)
  )
})

test_that("strata sizes add up", {
  s <- c(1, 2, 3, 2, 3, 1, 3, 2, 1, 3, 3, 1, 2, 1, 1, 2, 3, 1, 3, 2, 2)
  s <- factor(s, 1:4)
  x <- c(1:10, 10:0)
  alloc <- prop_allocation(x, 11, s)
  samp <- sps(x, alloc, s)
  expect_identical(
    tabulate(s[samp], nbins = 4),
    as.vector(alloc)
  )
})

test_that("permanent random numbers work", {
  set.seed(4321)
  prn <- runif(11)
  x <- c(100, 1:9, 100)
  expect_identical(
    sps(x, 5, prn = prn),
    sps(x, 5, prn = prn)
  )
  set.seed(4321)
  expect_identical(
    sps(x, 5, prn = prn),
    sps(x, 5)
  )
  set.seed(4321)
  expect_identical(
    ps(x, 5, prn = prn),
    ps(x, 5)
  )

  expect_identical(
    as.integer(sps(c(x, x), c(5, 4), gl(2, 11), prn = c(prn, prn))),
    c(sps(x, 5, prn = prn), (12:22)[sps(x, 4, prn = prn)])
  )
})

test_that("extending a stratified sample works", {
  set.seed(1432)
  u <- runif(100)
  x <- c(runif(98), 100, 200)
  samp <- sps(x, c(5, 6), rep(1:2, each = 50), u)
  drop <- c(10, 100, 54)
  samp2 <- sps(x[-drop], c(4, 4), rep(1:2, each = 50)[-drop], u[-drop])
  expect_identical(
    x[samp[-match(drop, samp)]],
    x[-drop][samp2]
  )
})

test_that("top-up sampling works", {
  set.seed(15243)
  u <- runif(10)
  x <- 1:10
  expect_true(all(sps(x, 4, prn = u)[1:4] %in% sps(x, 5, prn = u)))
})

test_that("pareto order sampling works", {
  pareto <- order_sampling(function(x) x / (1 - x))

  u <- runif(20)
  expect_identical(
    as.vector(pareto(rep(1, 20), c(5, 6), rep(1:2, 10), u)),
    sort(
      c(
        seq(1L, 20L, 2L)[order(u[seq(1L, 20L, 2L)])[1:5]],
        seq(2L, 20L, 2L)[order(u[seq(2L, 20L, 2L)])[1:6]]
      )
    )
  )
  # shift prns
  u <- 1:9 / 10
  v <- (u - 0.49) %% 1

  expect_identical(
    as.integer(pareto(rep(1, 9), 5, prn = u)),
    1:5
  )
  expect_identical(
    as.integer(pareto(rep(1, 9), 5, prn = v)),
    5:9
  )
})

test_that("ties are broken by position", {
  x <- c(4, 1, 3, 2, 4)
  expect_identical(
    as.vector(sps(x, 3, prn = inclusion_prob(x, 3))),
    1:3
  )
})

test_that("cutoff units are included", {
  x <- c(4, 1, 3, 2, 4)
  expect_true(all(c(1, 5) %in% sps(x, 3, cutoff = 4)))
})

test_that("attributes get removed", {
  samp <- sps(1:5, 3)
  # mathematical functions should treat 'sps' objects as numeric vectors
  expect_true(inherits(log(samp), "numeric"))
  expect_true(inherits(1L + samp, "integer"))
  expect_true(inherits(samp / 2, "numeric"))
  expect_true(inherits(samp > samp, "logical"))
  expect_true(inherits(-samp, "integer"))

  # and replacement methods
  expect_true(inherits(replace(samp, 1, 1), "numeric"))
  expect_true(inherits(replace(samp, 1, 1L), "integer"))

  length(samp) <- 2
  expect_true(inherits(samp, "integer"))
})

test_that("agrees with manual calculation", {
  set.seed(12345)
  prn <- c(0.99, runif(9))
  x <- c(20, 1:5, 7, 6, 100, 8)
  expect_identical(
    as.integer(sps(x, 5, prn = prn)),
    sort(c(1L, 9L, c(2:8, 10L)[order(prn[-c(1, 9)] / x[-c(1, 9)])[1:3]]))
  )

  expect_identical(
    as.integer(ps(x, 5, prn = prn)),
    sort(which(prn < inclusion_prob(x, 5)))
  )
})
