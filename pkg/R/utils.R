#' Select columns using a regular expression
#'
#' This function selects columns of an R object with column names
#' (for example a \code{\link[base]{data.frame}}, \code{\link[base]{matrix}},
#' \code{\link[stats]{ts}} or \code{\link{regts}}).
#' The columns with names matching a given regular expression are selected.
#' This function employs base R function \code{\link{grep}}
#' @param x an R object with column names (e.g. a \code{data.frame},
#' \code{matrix}, \code{ts} or \code{regts})
#' @param regex a regular expression used to select a column
#' @param drop if \code{TRUE}, the result is coerced to a vector if possible
#' @param ... arguments passed to \link{grep}
#' @return the column selection of object \code{x}
#' @examples
#' data <- regts(matrix(1:20, ncol = 4), start = "2010Q2",
#'               names = c("nlc", "ukc", "nly", "uky"))
#'
#' # select all columns with names starting with nl
#' nl_data <- select_columns(data, "nl.*")
#' @export
select_columns <- function(x, regex, drop = TRUE, ...) {
  cnames <- colnames(x)
  if (is.null(cnames)) {
    stop("No column names available. No selection possible")
  }
  sel <- grep(regex, cnames, ...)
  return (x[ , sel, drop = drop])
}

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
