#' Select columns using a regular expression
#'
#' This function selects columns of an R object with column names
#' (for example a \code{\link{data.frame}}, \code{\link{matrix}} or
#' \code{\link[stats]{ts}} or \code{\link{regts}}).
#' The columns with names matching a given regular expression are selected.
#' @param x an R object with column names (e.g. a \code{data.frame},
#' \code{matrix}, \code{ts} or \code{regts})
#' @param regex a regular expression used to select column
#' @param drop if \code{TRUE}, the result is coerced to a vector if possible.
#' @return the column selection of object \code{x}
#' @examples
#' data <- regts(matrix(1:20, ncol = 4), start = "2010Q2",
#'               names = c("nlc", "ukc", "nly", "uky"))
#' nl_data <- select_columns(data, "nl.*")
#' @export
select_columns <- function(x, regex, drop = TRUE) {
    cnames <- colnames(x)
    if (is.null(cnames)) {
        stop("No column names available. No selection possible")
    }
    sel <- grep(regex, cnames)
    return (x[ , sel, drop = drop])
}
