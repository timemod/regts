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

# check argument frequency of functions such as as.period and as.period_range
check_frequency_arg <- function(frequency, arg_name = "frequency") {
  if (missing(frequency)) return(invisible())
  f <- frequency
  if (length(f) > 1 ||
        !(is.numeric(f) || identical(f, NA)) ||
        (!is.na(f) && f != as.integer(f))) {
    stop(sprintf("Argument '%s' should be a scalar integer value.",
                 arg_name))
  }
  return(invisible())
}
