set.seed(123454)

test_that("corner cases", {
  expect_identical(
    unclass(sps(1:10, 0)),
    structure(integer(0), weights = numeric(0), levels = character(0))
  )
  expect_identical(
    unclass(sps(1:10, 10)),
    structure(1:10, weights = rep(1, 10), levels = rep("TA", 10))
  )
  expect_identical(
    unclass(sps(1:10, c(5, 0), gl(2, 5))),
    structure(1:5, weights = rep(1, 5), levels = rep("TA", 5))
  )
  expect_identical(
    unclass(ps(1:10, 0)),
    structure(integer(0), weights = numeric(0), levels = character(0))
  )
  expect_identical(
    unclass(ps(1:10, 10)),
    structure(1:10, weights = rep(1, 10), levels = rep("TA", 10))
  )
  expect_identical(
    unclass(ps(1:10, c(5, 0), gl(2, 5))),
    structure(1:5, weights = rep(1, 5), levels = rep("TA", 5))
  )
})

test_that("argument checking", {
  expect_error(sps(-1:6, c(2, 2), gl(2, 3)))
  expect_error(sps(c(NA, 1:6), c(2, 2), gl(2, 3)))
  expect_error(sps(numeric(0), c(2, 2), gl(2, 3)))
  expect_error(sps(c(0, 0, 1:4), c(2, 2), gl(2, 3)))
  expect_error(sps(c(0, 0, 1:4), 5))
  expect_error(sps(1:6, c(-2, 2), gl(2, 3)))
  expect_error(sps(1:6, c(NA, 2), gl(2, 3)))
  expect_error(sps(1:6, integer(0), gl(2, 3)))
  expect_error(sps(1:6, 2, gl(2, 3)))
  expect_error(sps(1:6, c(2, 2)))
  expect_error(sps(1:6, c(2, 2), gl(2, 2)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3)[c(1:5, 7)]))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), alpha = c(0, 1)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), alpha = c(0, NA)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), alpha = c(0, 0, 0)))
  expect_error(sps(1:6, c(2, 2), gl(2, 2), alpha = integer(0)))
  expect_error(sps(1:6, 2, alpha = c(0, 0)))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), prn = 1:7 / 10))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), prn = -1:4 / 10))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), prn = c(NA, 1:5) / 10))
  expect_error(sps(1:6, c(2, 2), gl(2, 3), prn = numeric(0)))
})

test_that("results should be sorted", {
  x <- c(20, 1:10, 100, 0, 0)
  samp <- sps(x, c(3, 2, 2), c(1, 1, 2, 1, 3, 1, 2, 3, 2, 1, 3, 3, 3, 1))
  expect_identical(
    as.integer(samp),
    sort(samp)
  )
  # Weights should be monotonic
  expect_identical(
    order(weights(sps(0:10, 4))), 4:1
  )
})

test_that("two rounds of TA removal", {
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
  # Use alpha to make all units TAs
  expect_identical(
    levels(sps(c(0:5, 0:5), c(3, 3), rep(1:2, each = 6), alpha = c(0.51, 0))),
    c(rep("TA", 3), "TS", "TS", "TA")
  )
  # Does noting when units are already TAs
  expect_identical(
    sps(0:5, 5),
    sps(0:5, 5, alpha = 0.9)
  )
})

test_that("strata sizes should add up", {
  s <- c(1, 2, 3, 2, 3, 1, 3, 2, 1, 3, 3, 1, 2, 1, 1, 2, 3, 1, 3, 2, 2)
  x <- c(1:10, 10:0)
  alloc <- prop_allocation(x, 11, s)
  samp <- sps(x, alloc, s)
  expect_identical(
    tabulate(s[samp], nbins = 3), 
    as.vector(alloc)
  )
})

test_that("permanent random numbers", {
  set.seed(4321)
  prn <- runif(11)
  expect_identical(
    sps(c(100, 1:9, 100), 5, prn = prn), 
    sps(c(100, 1:9, 100), 5, prn = prn)
  )
  set.seed(4321)
  expect_identical(
    sps(c(100, 1:9, 100), 5, prn = prn), 
    sps(c(100, 1:9, 100), 5)
  )
  set.seed(4321)
  expect_identical(
    ps(c(100, 1:9, 100), 5, prn = prn), 
    ps(c(100, 1:9, 100), 5)
  )
})

test_that("extend a stratified sample", {
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

test_that("pareto order sampling", {
  pareto <- order_sampling(function(x) x / (1 - x))
  
  u <- runif(20)
  expect_identical(
    as.vector(pareto(rep(1, 20), c(5, 6), rep(1:2, 10), u)),
    sort(c(seq(1L, 20L, 2L)[order(u[seq(1L, 20L, 2L)])[1:5]], seq(2L, 20L, 2L)[order(u[seq(2L, 20L, 2L)])[1:6]]))
  )
  # Shift prns
  u <- 1:9 / 10
  v <- (u - 0.5) %% 1
  
  expect_identical(
    as.integer(pareto(rep(1, 9), 5, prn = u)),
    1:5
  )
  expect_identical(
    as.integer(pareto(rep(1, 9), 5, prn = v)),
    5:9
  )
})

test_that("strip attributes", {
  samp <- sps(1:5, 3)
  # Mathematical functions should treat 'sps' objects as numeric vectors
  expect_true(inherits(log(samp), "numeric"))
  expect_true(inherits(1L + samp, "integer"))
  expect_true(inherits(samp / 2, "numeric"))
  expect_true(inherits(samp > samp, "logical"))
  expect_true(inherits(-samp, "integer"))
  
  # And replacement methods
  expect_true(inherits(replace(samp, 1, 1), "numeric"))
  expect_true(inherits(replace(samp, 1, 1L), "integer"))
})
