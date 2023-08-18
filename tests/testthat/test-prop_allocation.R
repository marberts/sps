set.seed(4321)

test_that("corner cases for allocations work as expected", {
  expect_identical(
    prop_allocation(numeric(0), 0, factor(integer(0), levels = 1)),
    c("1" = 0L)
  )
  expect_identical(
    prop_allocation(0, 0, 1),
    c("1" = 0L)
  )
  expect_identical(
    prop_allocation(1:3, 0, 1:3),
    c("1" = 0L, "2" = 0L, "3" = 0L)
  )
  expect_identical(
    prop_allocation(c(1, 1, 0, 0), 2, gl(2, 2), divisor = identity),
    c("1" = 2L, "2" = 0L)
  )
  expect_identical(
    prop_allocation(1:5, 5, gl(1, 5)),
    c("1" = 5L)
  )
  expect_identical(
    prop_allocation(
      rep(1, 10), 4, factor(rep(letters[1], 10), levels = c("a", "b"))
    ),
    c(a = 4L, b = 0L)
  )
  expect_identical(
    prop_allocation(1:4, 2, gl(1, 4), initial = 3),
    c("1" = 2L)
  )
})

test_that("argument checking works for allocations", {
  expect_error(prop_allocation(c(-1, 1, 0), 1, c(1, 1, 2)))
  expect_error(prop_allocation(c(1, 1, NA), 1, c(1, 1, 2)))
  expect_error(prop_allocation(c(1, 1, 0), -1, c(1, 1, 2)))
  expect_error(prop_allocation(c(1, 1, 0), integer(0), c(1, 1, 2)))
  expect_error(prop_allocation(c(1, 1, 0), NA, c(1, 1, 2)))
  expect_error(prop_allocation(c(1, 1, 0), 3, c(1, 1, 2)))
  expect_error(prop_allocation(c(1, 1, 0), 1, c(NA, 1, 2)))
  expect_error(prop_allocation(c(1, 1, 0), 1, c(1, 1)))
  expect_error(prop_allocation(integer(0), 0, factor(integer(0))))
  expect_error(prop_allocation(c(1, 1, 0), 1, c(1, 1, 2), initial = integer(0)))
  expect_error(prop_allocation(c(1, 1, 0), 1, c(1, 1, 2), initial = c(1, 1)))
  expect_error(prop_allocation(c(1, 1, 0), 1, c(1, 1, 2), initial = c(1, -1)))
  expect_error(prop_allocation(c(1, 1, 0), 1, c(1, 1, 2), initial = c(2, 0)))
  expect_error(prop_allocation(c(1, 1, 0), 1, c(1, 1, 2), initial = c(2, 0, 1)))
  expect_error(prop_allocation(c(1, 1, 0), 1, c(1, 1, 2), divisor = "a"))
  expect_error(prop_allocation(c(1, 0, 1), 2, c(1, 1, 2), initial = c(2, 0)))
})

test_that("simple allocations are correct", {
  expect_identical(
    prop_allocation(rep(1, 10), 4, rep(letters[1:2], 5)),
    c(a = 2L, b = 2L)
  )
  expect_identical(
    prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5)),
    c(a = 0L, b = 4L)
  )
  expect_identical(
    prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5), initial = 1),
    c(a = 1L, b = 3L)
  )
  expect_identical(
    prop_allocation(rep(c(1, 10), 5), 4, rep(letters[1:2], 5), initial = 2),
    c(a = 2L, b = 2L)
  )
  expect_identical(
    prop_allocation(
      c(rep(10, 8), 1, 1), 5, c(rep("a", 8), "b", "b"), initial = 3
    ),
    c(a = 3L, b = 2L)
  )
  expect_identical(
    prop_allocation(
      rep(c(1, 10), 5), 4, rep(letters[1:2], 5), initial = c(3, 1)
    ),
    c(a = 3L, b = 1L)
  )
  expect_identical(
    prop_allocation(
      rep(c(1, 10), 5), 4, factor(rep(letters[1:2], 5), levels = letters[1:3]),
      initial = c(2, 1, 0)
    ),
    c(a = 2L, b = 2L, c = 0L)
  )
  expect_identical(
    prop_allocation(c(0, rep(1, 100), 0), 10, rep(1:4, c(11, 20, 30, 41))),
    c("1" = 1L, "2" = 2L, "3" = 3L, "4" = 4L)
  )
  expect_identical(
    prop_allocation(c(0, 0, 100, 1, 1, 1), 3, gl(2, 3)),
    c("1" = 1L, "2" = 2L)
  )
  expect_identical(
    prop_allocation(
      c(1, 1, 1, 1, 1, 1, 1, 10, 10, 10), 5,
      rep(letters[1:2], c(7, 3))
    ),
    c(a = 2L, b = 3L)
  )
  expect_identical(
    prop_allocation(
      c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5,
      rep(letters[1:3], c(6, 2, 2))
    ),
    c(a = 1L, b = 2L, c = 2L)
  )
  expect_identical(
    prop_allocation(
      c(1, 1, 1, 1, 1, 1, 9, 9, 100, 100), 5,
      rep(letters[1:3], c(6, 2, 2)), divisor = identity
    ),
    c(a = 1L, b = 2L, c = 2L)
  )
})

test_that("ties are broken correctly", {
  expect_identical(
    prop_allocation(rep(1, 9), 8, rep(1:2, c(4, 5))),
    c("1" = 3L, "2" = 5L)
  )
  expect_identical(
    prop_allocation(rep(1, 9), 8, rep(1:2, c(4, 5)), ties = "first"),
    c("1" = 4L, "2" = 4L)
  )
  expect_identical(
    prop_allocation(rep(1, 5), 2, 1:5),
    c("1" = 1L, "2" = 1L, "3" = 0L, "4" = 0L, "5" = 0L)
  )
  expect_identical(
    prop_allocation(rep(1, 5), 2, 1:5, ties = "first"),
    c("1" = 1L, "2" = 1L, "3" = 0L, "4" = 0L, "5" = 0L)
  )
})

test_that("alabama paradox doesn't happen", {
  expect_identical(
    prop_allocation(rep(1, 14), 10, rep(1:3, c(6, 6, 2))),
    c("1" = 5L, "2" = 4L, "3" = 1L)
  )
  expect_identical(
    prop_allocation(rep(1, 14), 11, rep(1:3, c(6, 6, 2))),
    c("1" = 5L, "2" = 5L, "3" = 1L)
  )
})

test_that("allocations are correct for voting examples", {
  # example from https://en.wikipedia.org/wiki/Highest_averages_method
  x <- rep(1, 1e5)
  s <- factor(
    rep(1:7, c(47000, 16000, 15900, 12000, 6000, 0, 3100)),
    levels = 1:7
  )

  expect_identical(
    prop_allocation(x, 10, s),
    c("1" = 5L, "2" = 2L, "3" = 2L, "4" = 1L, "5" = 0L, "6" = 0L, "7" = 0L)
  )
  expect_identical(
    prop_allocation(x, 10, s, divisor = \(a) a + 0.5),
    c("1" = 4L, "2" = 2L, "3" = 2L, "4" = 1L, "5" = 1L, "6" = 0L, "7" = 0L)
  )
  expect_identical(
    prop_allocation(x, 10, s, divisor = \(a) sqrt(a * (a + 1))),
    c("1" = 4L, "2" = 2L, "3" = 1L, "4" = 1L, "5" = 1L, "6" = 0L, "7" = 1L)
  )
  expect_identical(
    prop_allocation(x, 10, s, divisor = \(a) a),
    c("1" = 3L, "2" = 2L, "3" = 2L, "4" = 1L, "5" = 1L, "6" = 0L, "7" = 1L)
  )
})

test_that("expected coverage works", {
  expect_equal(expected_coverage(0, 0, 1), 0)
  expect_equal(expected_coverage(1:6, 6, gl(1, 6)), 1)
  expect_equal(expected_coverage(1:6, 0, gl(1, 6)), 0)
  expect_equal(expected_coverage(1:6, 3, 1:6), 3)
  expect_equal(
    expected_coverage(1:10, 4, gl(2, 5)),
    expected_coverage(1:10, 4, gl(2, 5, labels = 1:3))
  )
  # bernoulli sampling
  expect_equal(
    expected_coverage(rep(1, 10), 4, c(rep(1, 4), rep(2, 6))),
    2 - (1 - 0.4)^4 - (1 - 0.4)^6
  )
  # simulation
  x <- c(
    0, 20, 16, 32, 14, 35, 9, 6, 2, 33, 29, 40, 27, 38, 47, 26, 46,
    12, 11, 39, 24, 100, 0, 1, 6, 6, 9, 20, 15, 25, 14, 0, 100
  )
  s <- c(
    4, 4, 2, 5, 5, 5, 5, 2, 3, 2, 5, 2, 2, 5, 5, 2, 3, 4, 5, 5, 3, 4,
    2, 2, 2, 3, 5, 2, 1, 1, 2, 3, 3
  )

  expect_equal(expected_coverage(x, 10, s), 4.41282552802)

  # simulate mean(replicate(1e3, length(unique(s[ps(x, 10)]))))
})

test_that("argument checking for expected coverage works", {
  expect_error(expected_coverage(numeric(0), 0, integer(0)))
  expect_error(expected_coverage(-1:4, 3, gl(2, 3)))
  expect_error(expected_coverage(c(1:5, NA), 3, gl(2, 3)))
  expect_error(expected_coverage(numeric(0), 3, gl(2, 3)))
  expect_error(expected_coverage(1:6, -3, gl(2, 3)))
  expect_error(expected_coverage(1:6, integer(0), gl(2, 3)))
  expect_error(expected_coverage(1:6, NA, gl(2, 3)))
  expect_error(expected_coverage(1:6, 3, gl(2, 2)))
  expect_error(expected_coverage(1:6, 3, gl(2, 3)[c(1:5, 7)]))
  expect_error(expected_coverage(1:6, 3, gl(2, 3), alpha = -0.5))
  expect_error(expected_coverage(1:6, 3, gl(2, 3), alpha = c(0, 1)))
  expect_error(expected_coverage(1:6, 3, gl(2, 3), alpha = numeric(0)))
  expect_error(expected_coverage(1:6, 3, gl(2, 3), alpha = NA))
})
