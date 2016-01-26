#' Convert a regts to a list of univariate regts objects
#' @param x a \code{regts} object
#' @return a list of univariate \code{regts} objects
as.list.regts <- function(x, ...) {
    x <- NextMethod("as.list")
    lapply(x, FUN = as.regts)
}
