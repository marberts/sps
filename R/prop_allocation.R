#---- Internal helpers ----
# Rounding methods
largest_remainder <- function(p, n, initial) {
  np <- p * (n / sum(p))
  npf <- floor(np)
  remainder <- rank(npf - np, ties.method = "first") <= n - sum(npf)
  initial + npf + remainder
}

highest_averages <- function(divisor) {
  divisor <- switch(
    divisor,
    "D'Hondt" = function(x) x + 1,
    "Webster" = function(x) x + 0.5,
    "Imperiali" = function(x) x + 2,
    "Huntington-Hill" = function(x) sqrt(x * (x + 1)),
    "Danish" = function(x) x + 1 / 3,
    "Adams" = function(x) x,
    "Dean" = function(x) x * (x + 1) / (x + 0.5)
  )
  # return function
  function(p, n, initial) {
    if (is.null(names(initial))) {
      names(initial) <- names(p)
    }
    while (n > 0) {
      i <- which.max(p / divisor(initial))
      initial[i] <- initial[i] + 1
      n <- n - 1
    }
    initial
  }
}

#---- Expected coverage ----
expected_coverage <- function(x, N, s = gl(1, length(x))) {
  if (not_strict_positive_vector(x)) {
    stop(
      gettext("'x' must be a strictly positive and finite numeric vector")
    )
  }
  N <- trunc(N)
  if (not_positive_number(N)) {
    stop(
      gettext("'N' must be a positive and finite number")
    )
  }
  if (N > length(x)) {
    stop(
      gettext("sample size 'N' is greater than or equal to population size")
    )
  }
  s <- as.factor(s)
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 's' must be the same length")
    )
  }
  if (anyNA(s)) {
    stop(
      gettext("'s' cannot contain NAs")
    )
  }
  p <- split(log(1 - .inclusion_prob(x, N)), s)
  sum(1 - vapply(p, function(x) exp(sum(x)), numeric(1L)))
}

#---- Proportional allocation ----
prop_allocation <- function(
    x, N, s = gl(1, length(x)), min = 0, 
    method = c("Largest-remainder", "D'Hondt", "Webster", "Imperiali", 
               "Huntington-Hill", "Danish", "Adams", "Dean")  
) {
  if (not_strict_positive_vector(x)) {
    stop(
      gettext("'x' must be a strictly positive and finite numeric vector")
    )
  }
  N <- trunc(N)
  if (not_positive_number(N)) {
    stop(
      gettext("'N' must be a positive and finite number")
    )
  }
  min <- trunc(min)
  if (not_positive_number(min)) {
    stop(
      gettext("'min' must be a positive and finite number")
    )
  }
  if (N > length(x)) {
    stop(
      gettext("sample size 'N' is greater than or equal to population size")
    )
  }
  s <- as.factor(s)
  if (length(x) != length(s)) {
    stop(
      gettext("'x' and 's' must be the same length")
    )
  }
  if (anyNA(s)) {
    stop(
      gettext("'s' cannot contain NAs")
    )
  }
  method <- match.arg(method)
  ns <- tabulate(s, nbins = nlevels(s))
  if (any(min > ns)) {
    stop(
      gettext("'min' must be smaller than the population size for each stratum")
    )
  }
  N <- N - min * nlevels(s)
  if (N < 0) {
    stop(
      gettext("minimal allocation is larger than 'N'")
    )
  }
  # apportionment method
  round <- if (method == "Largest-remainder") {
    largest_remainder
  } else {
    highest_averages(method)
  }
  p <- vapply(split(x, s), sum, numeric(1L))
  res <- structure(rep(min, length(p)), names = levels(s))
  # redistribute sample units for those that cap out at the stratum size
  repeat {
    res <- round(p, N, res)
    d <- pmax(res - ns, 0)
    over <- as.logical(d)
    if (!any(over)) break
    res[over] <- ns[over]
    p[over] <- 0  
    N <- sum(d)
  }
  res
}
