.onLoad <- function(libname, pkgname) {
  options(sps.usekit = requireNamespace("kit", quietly = TRUE))
}
