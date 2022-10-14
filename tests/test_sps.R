library(sps)

set.seed(1234)

# Corner cases
unclass(sps(1, 0)) 
unclass(sps(integer(0), integer(0)))
sps(1, 1)
setequal(sps(1:10, 10), 1:10)
weights(sps(1:10, 10))
levels(sps(1:10, 10))
all(weights(sps(1:10, 1)) >= 5.5)
sps(1:4, c(2, 0), c(1, 1, 2, 2))
levels(sps(1:4, c(1, 2), c(1, 1, 2, 2)))

# Two rounds of TA removal
sps(c(20, 1:10, 100), 5)[c(1, 5)]
levels(sps(c(20, 1:10, 100), 5))

# One round of TA removal
sps(c(1:10, 90, 100), 5)[4:5]
levels(sps(c(1:10, 90, 100), 5))

# Return value should be an integer
is.integer(sps(1:5, 3))
is.integer(sps(1, 0))
is.integer(sps(1, 1))
is.integer(sps(1:4, c(1, 2), c(1, 1, 2, 2)))

# Strata sizes should add up
s <- factor(sample(letters, 100, TRUE), letters)
x <- runif(100)
alloc <- prop_allocation(x, 50, s)
res <- s[sps(x, alloc, s)]
all.equal(tabulate(factor(res, letters)), as.vector(alloc))

# Tests for permanent random numbers
set.seed(4321)
prn <- runif(10)
all.equal(sps(c(1:9, 100), 5, prn = prn), sps(c(1:9, 100), 5, prn = prn))
set.seed(4321)
all.equal(sps(1:10, 5, prn = prn), sps(1:10, 5))

# Test for extending a stratified sample
set.seed(1432)
u <- runif(100)
x <- c(runif(98), 100, 200)
samp <- sps(x, c(5, 6), rep(1:2, each = 50), u)
drop <- c(10, 100, 54)
samp2 <- sps(x[-drop], c(4, 4), rep(1:2, each = 50)[-drop], u[-drop])
all.equal(x[samp[-match(drop, samp)]], x[-drop][samp2])

# Mathematical functions should treat 'sps' objects as numeric vectors
str(log(samp))
str(1L + samp)
str(samp / 2)
str(samp > samp)
str(-samp)

# And replacement methods
samp[1] <- 1
str(samp)

# Tests for error messages
try(sps(-1, 1))
try(sps(1, NA))
try(sps(1:4, 2, c(1, 1, 2, 2)))
try(sps(1:4, c(5, 2), c(1, 1, 2, 2)))
try(sps(1:4, 2, prn = c(0.1, 0.1, 0.1, NA)))
