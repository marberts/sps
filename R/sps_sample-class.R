#' Constructor for sps samples
#' @noRd
new_sps_sample <- function(x, w) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(w))
  structure(x, weights = w, class = c("sps_sample", "numeric"))
}

#' Methods for sps objects
#' @noRd
#' @export
levels.sps_sample <- function(x) {
  res <- rep.int("TS", length(x))
  res[weights(x) == 1] <- "TA"
  res
}

#' @export
`levels<-.sps_sample` <- function(x, value) {
  stop("cannot replace levels attribute")
}

#' @export
`length<-.sps_sample` <- function(x, value) {
  x <- as.vector(x)
  NextMethod()
}

#' @export
#' @importFrom stats weights
weights.sps_sample <- function(object, ...) {
  chkDots(...)
  attr(object, "weights")
}

#' @export
print.sps_sample <- function(x, ...) {
  print(as.vector(x), ...)
  invisible(x)
}

#' @export
summary.sps_sample <- function(object, ...) {
  chkDots(...)
  n <- length(object)
  ts <- sum(weights(object) > 1)
  structure(list(n = n, ts = ts, ta = n - ts), class = "sps_sample_summary")
}

#' @export
print.sps_sample_summary <- function(x, ...) {
  chkDots(...)
  cat(
    "Sample of", x$n, "units with", x$ta,
    "take-all unit and", x$ts, "take-some units"
  )
  invisible(x)
}

#' @export
Math.sps_sample <- function(x, ...) {
  x <- as.vector(x)
  NextMethod()
}

#' @export
Ops.sps_sample <- function(e1, e2) {
  if (inherits(e1, "sps_sample")) e1 <- as.vector(e1)
  if (nargs() == 2L && inherits(e2, "sps_sample")) e2 <- as.vector(e2)
  NextMethod()
}

#' @export
`[<-.sps_sample` <- function(x, i, value) {
  x <- as.vector(x)
  NextMethod()
}

#' @export
`[[<-.sps_sample` <- function(x, i, value) {
  x <- as.vector(x)
  NextMethod()
}
