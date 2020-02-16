univec2unimat <- function(x, x_name) {
  # function converts a univariate vector timeseries based
  # to a univariate matrix timeseries.
  dim(x) <- c(length(x), 1)
  colnames(x) <- x_name
  return(x)
}
