library(sps)

#all.equal(expected_coverage(numeric(0), integer(0)), 0)
all.equal(expected_coverage(1:6, 6), 1)
all.equal(expected_coverage(1:6, 0), 0)
all.equal(expected_coverage(1:6, 3, 1:6), 3)
all.equal(
  expected_coverage(1:10, 4, gl(2, 5)),
  expected_coverage(1:10, 4, gl(2, 5, labels = 1:3))
)
