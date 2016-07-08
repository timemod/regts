#' Select columns using  a filter
#'
#' This function selects columns of R objects that support column
#' selection (e.g. a \code{data.frame}, \code{matrix}, \code{ts} or
#' \code{regts})  using a
#' regular expression.
#' @param x any R object that supports column selection (\code{data.frame},
#' \code{matrix}, \code{ts}, \code{regts})
#' @param regex a regular expression used to select column
#' @return the selection of object \code{x}
#' @export
filter_columns <- function(x, regex) {
    sel <- grep(regex, colnames(x))
    return (x[ , sel, drop = FALSE])
}
