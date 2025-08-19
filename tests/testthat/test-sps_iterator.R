test_that("streaming is the same as drawing", {
  set.seed(13026)
  x <- rlnorm(10)
  u <- runif(10)
  res <- integer(0)
  s <- sps_iterator(x, prn = u)
  for (i in 1:4) {
    res <- c(res, s())
    expect_setequal(sps(x, i, prn = u), res)
  }
  for (i in 6:10) {
    res <- c(res, s())
    expect_setequal(sps(x, i, prn = u), res)
  }

  s <- sps_iterator(x, n = 5, prn = u)
  expect_setequal(c(3, 5), s())

  res <- c(2, 3, 4, 5, 6)
  s <- sps_iterator(x, n = 6, prn = u)
  for (i in 6:10) {
    res <- c(res, s())
    expect_setequal(sps(x, i, prn = u), res)
  }
})

test_that("NULL cases work", {
  s <- sps_iterator(1:5, n = 6)
  expect_null(s())

  s <- sps_iterator(integer(0))
  expect_null(s())
})

test_that("errors work", {
  expect_error(sps_iterator(1:5, 7))
  expect_error(sps_iterator(1:5, prn = 1:6))
  expect_error(sps_iterator(0))
})
