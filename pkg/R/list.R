#' Convert a \code{\link{regts}} to a list of univariate regts objects
#'
#' This function converts a \code{\link{regts}} to a list of univariate
#' \code{regts} objects.
#
#' @param x a \code{\link{regts} object}
#' @param ... arguments passed to methods (not used in the default
#' implementation)
#' @return a list of univariate \code{regts} objects
#' @examples
#' regts1 <- regts(matrix(1:6, ncol = 2), start = "2015Q3", names = c("a", "b"))
#'
#' # convert regts1 to a list
#' ts_list1 <- as.list(regts1)
#'
#' # use the within function to modify timeseries and create new timeseries
#' ts_list2 <- within (ts_list1, {
#'    b["2015q2"] <- 2
#'    c <- a * b
#'    d <- lag(c)
#'  })
#'
#' # use functions do.call and cbind to convert
#' # the list of timeseries objects to a multivariate regts
#' regts2 <- do.call(cbind, ts_list2)
#'
#' # transfer all timeseries in the list to the global environment
#' list2env(ts_list2, .GlobalEnv)
#' @seealso \code{\link[base]{list2env}} and  \code{\link{cbind}}
#' @name as.list
#' @export
as.list.regts <- function(x, ...) {

  if (!is.matrix(x)) {
    retval <- list(x)
    names(retval) <- deparse(substitute(x))
    return(retval)
  }

  if (is.null(colnames(x))) {
    # Create column names from the name of x
    xname <- deparse(substitute(x))
    nc <- NCOL(x)
    if (nc == 1) {
      colnames(x) <- xname
    } else {
      colnames(x) <- paste(xname, 1:nc, sep = ".")
    }
  }

  return(regts_to_list_rcpp(x))
}
