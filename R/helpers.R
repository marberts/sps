# Warning messages are annoying with length 0 inputs
.min <- function(x) if (length(x) == 0L) Inf else min(x)

.max <- function(x) if (length(x) == 0L) -Inf else max(x)
