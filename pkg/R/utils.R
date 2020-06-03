# converts an object to a character vector with strings.
# NAs are converted to ""
get_strings <- function(x) {
  ret <- as.character(x)
  ret[is.na(ret)] <- ""
  return(ret)
}

# convert a univariate vector timeseries to a univariate matrix time series,
# with the specified column name
univec2unimat <- function(x, name) {
  dim(x) <- c(length(x), 1)
  colnames(x) <- name
  return(x)
}
