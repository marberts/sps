library(sps)

set.seed(1234)

unclass(sps(1, 0)) 
unclass(sps(integer(0), integer(0)))
sps(1, 1)
setequal(sps(1:10, 10), 1:10)
weights(sps(1:10, 10))
levels(sps(1:10, 10))
sps(c(1:10, 20, 100), 5)[1:2]
levels(sps(c(1:10, 20, 100), 5))
sps(c(1:10, 90, 100), 5)[1:2]
levels(sps(c(1:10, 90, 100), 5))
all(weights(sps(1:10, 1)) >= 5.5)
sps(1:4, c(2, 0), c(1, 1, 2, 2))
levels(sps(1:4, c(1, 2), c(1, 1, 2, 2)))
