#' Faster order
#' @noRd
.topn <- function(x, n, decreasing = TRUE) {
  if (getOption("sps.usekit")) {
    kit::topn(x, n = n, decreasing = decreasing, hasna = FALSE)
  } else {
    order(x, decreasing = decreasing)[seq_len(n)]
  }
}

#' Validate that a factor represents sampling strata
#' @noRd
.validate_strata <- function(strata, x) {
  if (anyNA(strata)) {
    stop("cannot have missing strata")
  }
  if (nlevels(strata) < 1L) {
    stop("there must be at least one stratum")
  }
  if (length(x) != length(strata)) {
    stop("the vectors for sizes and strata must be the same length")
  }
  strata
}

#' Make random deviates
#' @noRd
.random_deviates <- function(prn, x) {
  if (is.null(prn)) {
    prn <- stats::runif(length(x))
  } else {
    prn <- as.numeric(prn)
    if (length(x) != length(prn)) {
      stop(
        "the vectors for sizes and permanent random numbers must be the ",
        "same length"
      )
    }
    if (any(prn <= 0) || any(prn >= 1)) {
      stop("permanent random numbers must be in (0, 1)")
    }
  }
  prn
}
