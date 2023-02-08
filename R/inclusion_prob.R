#---- Internal helpers ----
pi <- function(x, n) {
  # n == 0 should be a strong zero
  if (n == 0L) {
    rep.int(0, length(x))
  } else {
    x * (n / sum(x))
  }
}

# old version
# .inclusion_prob <- function(x, n) {
#   if (n > sum(x > 0)) {
#     stop(
#       gettext("sample size is greater than the number of units with non-zero sizes in the population")
#     )
#   }
#   res <- pi(x, n)
#   if (.max(res) > 1) {
#     repeat {
#       # inclusion probs weakly increase with each loop, so only need to 
#       # recalculate those strictly less than 1
#       
#       # if any inclusion prob is > 1 then there must be at least one 
#       # inclusion prob < 1
#       not_ta <- which(res < 1)
#       m <- n - length(x) + length(not_ta)
#       if (.max(res[not_ta] <- pi(x[not_ta], m)) <= 1) break
#     }
#     res <- pmin.int(res, 1)
#   }
#   res
# }

.inclusion_prob <- function(x, n, alpha) {
  res <- pi(x, n)
  if (.max(res) < 1 - alpha) return(res)
  # if x[n] == x[n + 1] after sorting then it is possible for the result 
  # to not resolve ties according to x (as documented) when alpha is large 
  # enough to make all units TAs; this is not practically relevant, as
  # alpha is usually small enough so that x[n] / sum(x) < 1 - alpha.
  # part <- order(x, decreasing = TRUE)
  part <- partition_index(x, n, decreasing = TRUE)
  s <- seq_len(n)
  possible_ta <- sort.int(part[s], decreasing = TRUE)
  definite_ts <- part[seq(n + 1, length.out = length(x) - n)]
  # order possible TA units according to x to make sub assignment easier,
  # noting that ties will be in reverse
  possible_ta <- possible_ta[order(x[possible_ta])]
  y <- x[possible_ta]
  p <- y * s / (sum(x[definite_ts]) + cumsum(y))
  ta <- possible_ta[p >= 1 - alpha]
  # faster than negative indexing
  ts <- c(definite_ts, setdiff(possible_ta, ta))
  res[ta] <- 1
  res[ts] <- pi(x[ts], n - length(ta))
  res
}

#---- Inclusion probability ----
inclusion_prob <- function(x, n, strata = NULL, alpha = 1e-4) {
  x <- as.numeric(x)
  if (.min(x) < 0) {
    stop(gettext("'x' must be greater than or equal to 0"))
  }
  
  n <- as.integer(n)
  if (length(n) == 0L) {
    stop(gettext("'n' cannot be length 0"))
  }
  if (min(n) < 0) {
    stop(gettext("'n' must be greater than or equal to 0"))
  }
  
  alpha <- as.numeric(alpha)
  if (length(alpha) == 0L) {
    stop(gettext("'alpha' cannot be length 0"))
  }
  if (min(alpha) < 0 || max(alpha) >= 1) {
    stop(gettext("'alpha' must be in [0, 1)"))
  }
  
  # the single stratum case is common enough to warrant the optimization
  if (is.null(strata)) {
    if (length(n) != 1L) {
      stop(gettext("cannot supply multiple sample sizes without strata"))
    }
    if (length(alpha) != 1L) {
      stop(gettext("cannot supply multiple values for 'alpha' without strata"))
    }
    if (n > sum(x > 0)) {
      stop(
        gettext("sample size is greater than the number of units with non-zero sizes in the population")
      )
    }
    .inclusion_prob(x, n, alpha)
  } else {
    strata <- as.factor(strata)
    if (length(x) != length(strata)) {
      stop(gettext("'x' and 'strata' must be the same length"))
    }
    if (length(n) != nlevels(strata)) {
      stop(
        gettext("'n' must have a single sample size for each level in 'strata'")
      )
    }
    # missing strata means inclusion probs are all missing
    if (anyNA(strata)) {
      stop(gettext("'strata' cannot contain NAs"))
    }
    if (length(alpha) != 1L && length(alpha) != nlevels(strata)) {
      stop(
        gettext("'alpha' must have a single value or a value for each level in 'strata'")
      )
    }
    
    x <- split(x, strata)
    # there must be at least one level for strata, so unlist can't return NULL
    if (any(unlist(Map(\(x, n) n > sum(x > 0), x, n), use.names = FALSE))) {
      stop(
        gettext("sample size is greater than the number of units with non-zero sizes in the population")
      )
    }
    
    unsplit(Map(.inclusion_prob, x, n, alpha), strata)
  }
}
