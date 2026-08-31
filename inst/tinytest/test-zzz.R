# Option to toggle {kit} works.
if (requireNamespace("kit", quietly = TRUE)) {
  x <- c(1, 4, 5, 10, 2, 3, 3, 1, 4)

  options(sps.usekit = TRUE)
  res1 <- inclusion_prob(x, 5)
  options(sps.usekit = FALSE)
  res2 <- inclusion_prob(x, 5)
  expect_equal(res1, res2)
}
