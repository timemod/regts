#' Convert a \code{\link{regts}} to a list of univariate regts objects
#'
#' This function converts a \code{\link{regts}} to a list of univariate
#' \code{regts} objects.
#
#' @param x a \code{regts} object
#' @param ... arguments passed to methods (not used in the default implementation)
#' @return a list of univariate \code{regts} objects
#' @examples
#'
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
#'  # convert ts_list2 to a multivariate regts
#'  regts2 <- do.call(cbind, ts_list2)
#'
#' @seealso The functions \code{\link{do.call}} and \code{\link{cbind}}
#' can be used to convert the list of timeseries objects to a multivariate
#' \code{regts} (see the example).
#'
#' @export
as.list.regts <- function(x, ...) {

    if (!is.matrix(x)) {
        retval <- list(x)
        names(retval) <- deparse(substitute(x))
        return (retval)
    }

    retval <- lapply(seq_len(ncol(x)), function(i) x[, i])

    # the colnames of the original timeseries become the names of the list
    cnames <- colnames(x)
    if (!is.null(cnames)) {
        names(retval) <- cnames
    } else {
        # The return value should always have names. Otherwise, a problem
        # could occur when the list is converted to a regts again
        # by using do.call(cbind, l)
        xname <- deparse(substitute(x))
        nc <- NCOL(x)
        if (nc == 1) {
            names(retval) <- xname
        } else {
            names(retval) <- paste(xname, 1:nc, sep = ".")
        }
    }
    return (retval)
}
