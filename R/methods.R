#---- Methods for class 'sample' ----
# levels.default() extracts levels; no need for a method

weights.sample <- function(object, ...) {
  attr(object, "weights")
}

print.sample <- function(x, ...) {
  print(as.numeric(x), ...)
}