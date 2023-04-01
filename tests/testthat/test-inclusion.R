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
  expect_error(inclusion_prob(1:6, 2, gl(2, 3)))
  expect_error(inclusion_prob(1:6, c(2, 2)))
  expect_error(inclusion_prob(1:6, c(2, 2), gl(2, 2)))
  expect_error(inclusion_prob(1:6, c(2, 2), gl(2, 3)[c(1:5, 7)]))
  expect_error(inclusion_prob(1:6, c(2, 2), gl(2, 3), alpha = c(0, 1)))
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
})

test_that("TAs are added with alpha", {
  # add more TAs with alpha
  x <- c(0, 4, 1, 4, 5)

  expect_equal(
    inclusion_prob(x, 3, alpha = 0.1),
    c(x[-5] / 9 * 2, 1)
  )
  expect_equal(
    inclusion_prob(x, 3, alpha = 0.15),
    c(x[1] / 5, 1, x[3:4] / 5, 1)
  )
  expect_equal(
    inclusion_prob(x, 3, alpha = 0.2),
    c(0, 1, 0, 1, 1)
  )
})
