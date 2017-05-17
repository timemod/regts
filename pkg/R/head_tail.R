#' @importFrom utils head
#' @export
head.regts <- function(x, n = 6L, ...) {

  first <- start_period(get_period_range(x))
  last  <- end_period(get_period_range(x))
  period <- period_range(first, min(first + n-1, last))

  return(x[period, ])

}

#' @importFrom utils tail
#' @export
tail.regts <- function(x, n = 6L, ...) {

  first <- start_period(get_period_range(x))
  last  <- end_period(get_period_range(x))
  period <- period_range(max(first,last - n+1), last)

  return(x[period, ])

}

#' Returns the topleft part of a regts.
#'
#' @param x a multivariate \code{\link{regts}}
#' @param n a single integer. Length period for the resulting object.
#' @param ncol a single integer. Number of columns in \code{\link{regts}}.
#' By default only the first 10 columns are printed.
#' @examples
#' data <- regts(matrix(1:200, ncol = 20), start = "2010Q2",
#'               names = paste0("abc", 1:20))
#' topleft(data)
#' @seealso
#' \code{\link{head}}, \code{\link{tail}}
#' @export
topleft <- function(x, n = 6L, ncol = 10L) {

  first <- start_period(get_period_range(x))
  last  <- end_period(get_period_range(x))
  period <- period_range(first, min(first + n-1, last))

  return(x[period, 1:ncol ])

}



