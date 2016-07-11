#' Convert a regts to a list of univariate regts objects
#' @param x a \code{regts} object
#' @param ... arguments passed to methods (not used in the default implementation)
#' @return a list of univariate \code{regts} objects
#' @examples
#' ts <- regts(matrix(1:6, ncol = 2), start = "2015Q3", names = c("a", "b"))
#' print(as.list(ts))
#' @export
as.list.regts <- function(x, ...) {
    retval <- lapply(seq_len(ncol(x)), function(i) x[, i])
    names(retval) <- colnames(x)
    return (retval)
}
