library(sps)

set.seed(1234)

# Corner cases
all.equal(
  unclass(sps_repweights(weights(sps(1:10, 10)), 5, tau = 3)),
  structure(matrix(1, 10, 5), tau = 3)
)
all.equal(
  unclass(sps_repweights(weights(sps(1:10, 10)), 10, dist = rnorm)),
  structure(matrix(1, 10, 10), tau = 1)
)

# All rep weights should be positive with the TA strata having all 1s
all(sps_repweights(1:5, tau = 2) > 0)
all(sps_repweights(1:5, tau = 2)[1, ] == 1)

# Check error messages
try(sps_repweights(1:5, B = 0.9))
try(sps_repweights(1:5, tau = NA))

# Printing should preserve names
sps_repweights(structure(rep(1, 5), names = letters[1:5]), 5)

# Check against bootstrapFP:::generalised()
# Fixed a bug with the exponential case by replaing exp() with rexp()
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

all.equal(var1, var2)
