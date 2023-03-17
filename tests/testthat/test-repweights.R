set.seed(1234)

test_that("corner cases work as expected", {
  w <- rep(1, 10)

  # all TA units gives a matrix of 1s
  expect_equal(
    sps_repweights(w, 5, tau = 3),
    structure(matrix(1, 10, 5), tau = 3)
  )
  expect_equal(
    sps_repweights(w, 5, dist = rnorm),
    structure(matrix(1, 10, 5), tau = 1)
  )

  # asking for 0 repweights gives a matrix with no columns
  expect_equal(
    sps_repweights(w, 0),
    structure(matrix(numeric(0), 10, 0), tau = 1)
  )
  expect_equal(
    sps_repweights(w, 0, dist = rnorm),
    structure(matrix(numeric(0), 10, 0), tau = 1)
  )

  # supplying no weights gives a matrix with no rows
  expect_equal(
    sps_repweights(integer(0), 5),
    structure(matrix(numeric(0), 0, 5), tau = 1)
  )
  expect_equal(
    sps_repweights(integer(0), 5, dist = rnorm),
    structure(matrix(numeric(0), 0, 5), tau = 1)
  )

  # intersection of both cases
  expect_equal(
    sps_repweights(integer(0), 0),
    structure(matrix(numeric(0), 0, 0), tau = 1)
  )
  expect_equal(
    sps_repweights(integer(0), 0, dist = rnorm),
    structure(matrix(numeric(0), 0, 0), tau = 1)
  )
})

test_that("argument checking works", {
  expect_error(sps_repweights(0:5, 5))
  expect_error(sps_repweights(c(NA, 1:5), 5))
  expect_error(sps_repweights(1:5, -5))
  expect_error(sps_repweights(1:5, NA))
  expect_error(sps_repweights(1:5, integer(0)))
  expect_error(sps_repweights(1:5, 5, -1))
  expect_error(sps_repweights(1:5, 5, NA))
  expect_error(sps_repweights(1:5, 5, numeric(0)))
})

test_that("rep weights works for TA units", {
  expect_true(all(sps_repweights(1:5, tau = 2) > 0))
  expect_true(all(sps_repweights(1:5, tau = 2)[1, ] == 1))
})

test_that("results agree with bootstrapFP:::generalised()", {
  # fixed a bug with the exponential case by replacing exp() with rexp()
  bootstrapFP_fixed <- function(ys, pks, B) {
    n <- length(ys)
    ht <- vector("numeric", length = B)
    w <- 1 / pks
    for (b in seq_len(B)) {
      a <- 1 + (rexp(n) - 1) * sqrt(1 - pks)
      ws <- a * w
      ht[b] <- sum(ys * ws)
    }
    HT <- sum(w * ys)
    (sum((ht - HT))^2) / B
  }

  w <- 1 / c(1, runif(98), 1)
  y <- rlnorm(100)

  set.seed(51423)
  rw <- sps_repweights(w, 100, dist = function(x) rexp(x) - 1)
  var1 <- sum(colSums(rw * y) - sum(w * y))^2 / 100

  set.seed(51423)
  var2 <- bootstrapFP_fixed(y, 1 / w, 100)

  expect_equal(var1, var2)
})
