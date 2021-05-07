set.seed(1234)

stopifnot(
  exprs = {
    identical(sps_repweights(weights(sps(1:10, 0)), 10), 
              structure(matrix(numeric(0), nrow = 0, ncol = 10), tau = 1, class = c("sps_brw")))
    identical(sps_repweights(weights(sps(1:10, 10)), 5), 
              structure(matrix(1, nrow = 10, ncol = 5), tau = 1, class = c("sps_brw")))
    identical(sps_repweights(weights(sps(1:10, 10)), 10, tau = 3), 
              structure(matrix(1, nrow = 10, ncol = 10), tau = 3, class = c("sps_brw")))
    identical(sps_repweights(weights(sps(1:10, 10)), 10, dist = rnorm), 
              structure(matrix(1, nrow = 10, ncol = 10), tau = 1, class = c("sps_brw")))
    replicate(100, all(sps_repweights(1:5, tau = 2) > 0))
    sps_repweights(1:5, tau = 2)[1, ] == 1
  },
  local = getNamespace("sps")
)
