#' Select columns using a filter
#'
#' This function selects columns of an R object that has columns names
#' (e.g. a \code{data.frame}, \code{matrix}, \code{ts} or
#' \code{regts})  using a
#' regular expression.
#' @param x any R object with column names (\code{data.frame},
#' \code{matrix}, \code{ts}, \code{regts})
#' @param regex a regular expression used to select column
#' @param drop if \code{TRUE}, the result is coerced to a vector if possible.
#' @return the selection of object \code{x}
#' @export
filter_columns <- function(x, regex, drop = TRUE) {
    cnames <- colnames(x)
    if (is.null(cnames)) {
        stop("No column names available. No selection possible")
    }
    sel <- grep(regex, colnames(x))
    return (x[ , sel, drop = drop])
}
