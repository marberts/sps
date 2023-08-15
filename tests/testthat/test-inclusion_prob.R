set.seed(14235)

test_that("corner cases work as expected", {
  expect_equal(
    inclusion_prob(0, 0),
    0
  )
  expect_equal(
    inclusion_prob(1:3, c(0, 1, 0), factor(c(2, 2, 2), levels = 1:3)),
    1:3 / 6
  )
  expect_equal(
    inclusion_prob(1:6, c(0, 3), c(1, 1, 2, 1, 2, 2)),
    c(0, 0, 1, 0, 1, 1)
  )
  expect_equal(
    inclusion_prob(numeric(0), c(0, 0), factor(integer(0), 1:2)),
    numeric(0)
  )
  expect_equal(
    inclusion_prob(rep(1, 6), c(2, 1), c(1, 1, 2, 1, 2, 2)),
    c(2, 2, 1, 2, 1, 1) / 3
  )
  expect_equal(
    inclusion_prob(c(0, 1, 1, 1 + 1e-4), 3),
    c(0, 1, 1, 1)
  )
  expect_equal(
    inclusion_prob(c(0, 1, 1, 1 - 1e-4), 3),
    c(0, 1, 1, 1)
  )
})

test_that("argument checking works", {
  expect_error(inclusion_prob(-1:4, c(2, 2), gl(2, 3)))
  expect_error(inclusion_prob(c(NA, 1:5), c(2, 2), gl(2, 3)))
  expect_error(inclusion_prob(numeric(0), c(2, 2), gl(2, 3)))
  expect_error(inclusion_prob(numeric(0), 0, factor(integer(0))))
  expect_error(inclusion_prob(c(0, 0, 1:4), c(2, 2), gl(2, 3)))
  expect_error(inclusion_prob(c(0, 0, 1:4), 5))
  expect_error(inclusion_prob(1:6, c(-2, 2), gl(2, 3)))
  expect_error(inclusion_prob(1:6, c(NA, 2), gl(2, 3)))
  expect_error(inclusion_prob(1:6, integer(0), gl(2, 3)))
  #expect_error(inclusion_prob(1:6, 2, gl(2, 3)))
  expect_error(inclusion_prob(1:6, c(2, 2)))
  expect_error(inclusion_prob(1:6, c(2, 2), gl(2, 2)))
  expect_error(inclusion_prob(1:6, c(2, 2), gl(2, 3)[c(1:5, 7)]))
  expect_error(inclusion_prob(1:6, c(2, 2), gl(2, 3), alpha = c(0, 1.5)))
  expect_error(inclusion_prob(1:6, c(2, 2), gl(2, 3), alpha = c(0, NA)))
  expect_error(inclusion_prob(1:6, c(2, 2), gl(2, 3), alpha = c(0, 0, 0)))
  expect_error(inclusion_prob(1:6, c(2, 2), gl(2, 3), alpha = integer(0)))
  expect_error(inclusion_prob(1:6, 2, alpha = c(0, 0)))
})

test_that("inclusion probs are correct with different rounds of TA removal", {
  # no rounds
  x <- c(0:4, 10:8, 5:7, 0)
  expect_equal(inclusion_prob(x, 4), x / 55 * 4)
  # one round
  x <- c(x, 100)
  expect_equal(inclusion_prob(x, 4), c(x[1:12] / 55 * 3, 1))
  # two rounds
  x <- c(20, x)
  expect_equal(inclusion_prob(x, 5), c(1, x[2:13] / 55 * 3, 1))
  # should agree with design weights
  samp <- sps(x, c(4, 3), gl(2, 7))
  expect_equal(
    1 / inclusion_prob(x, c(4, 3), gl(2, 7))[samp],
    weights(samp)
  )
  # strata should be independent
  expect_equal(
    inclusion_prob(x, c(4, 3), gl(2, 7)),
    c(inclusion_prob(x[1:7], 4), inclusion_prob(x[8:14], 3))
  )
})

test_that("results agree with sampling::inclusionprobabilities()", {
  expect_equal(
    inclusion_prob(1:20, 12),
    c(1:16 / 136 * 8, rep(1, 4))
  )
  # sampling::inclusionprobabilities() gives a warning
  expect_equal(
    inclusion_prob(0:20, 12),
    c(0:16 / 136 * 8, rep(1, 4))
  )
  expect_equal(
    inclusion_prob(c(1, 2, 5, 5, 5, 10, 4, 1), 6),
    c(0.25, 0.5, 1, 1, 1, 1, 1, 0.25)
  )
  
  # sampling::inclusionprob() != inclusion_prob() with this vector
  # with the default alpha
  x <- c(100, 25, 94, 23, 55, 6, 80, 65, 48, 76, 
         31, 99, 45, 39, 28, 18, 54, 78, 4, 33)
  expect_equal(
    inclusion_prob(x, 10),
    c(1, x[-1] / sum(x[-1]) * 9)
  )
  expect_equal(
    inclusion_prob(x, 10, alpha = 0),
    x / sum(x) * 10
  )
})

test_that("TAs are added with alpha", {
  x <- c(0, 4, 1, 4, 5)
  expect_equal(
    inclusion_prob(rep(x, 3), c(3, 3, 3), gl(3, 5), alpha = c(0.1, 0.15, 0.2)),
    c(x[-5] / 9 * 2, 1, 
      x[1] / 5, 1, x[3:4] / 5, 1, 
      0, 1, 0, 1, 1)
  )
  
  # partial ordering doesn't break ties correctly
  x <- c(1, 2, 2, 2, 3)
  expect_equal(
    inclusion_prob(rep(x, 3), c(3, 3, 3), gl(3, 5), alpha = c(0.15, 0.5, 0.6)),
    c(x[-5] / 7 * 2, 1,
      0.2, 1, 0.4, 0.4, 1,
      0, 1, 1, 0, 1)
  )
  
  # alpha = 1 adds TA units in order
  x <- c(4, 3, 4, 2, 1, 0)
  expect_equal(
    inclusion_prob(rep(x, 6), 0:5, gl(6, 6), 1),
    c(0, 0, 0, 0, 0, 0,
      1, 0, 0, 0, 0, 0,
      1, 0, 1, 0, 0, 0,
      1, 1, 1, 0, 0, 0,
      1, 1, 1, 1, 0, 0,
      1, 1, 1, 1, 1, 0)
  )
})

test_that("inclusion probs are a fixed point", {
  x <- 1:10
  p <- inclusion_prob(x, 5)
  expect_equal(p, inclusion_prob(p, 5))
  
  x <- c(0, 4, 1, 4, 5)
  p <- inclusion_prob(x, 3, alpha = 0.15)
  expect_equal(p, inclusion_prob(p, 3))
})

test_that("n and alpha recycle", {
  x <- 1:10
  expect_equal(
    inclusion_prob(x, 3, gl(2, 5)),
    inclusion_prob(x, c(3, 3), gl(2, 5))
  )
  expect_equal(
    inclusion_prob(x, 3, gl(2, 5), alpha = 0.5),
    inclusion_prob(x, 3, gl(2, 5), alpha = c(0.5, 0.5))
  )
})
