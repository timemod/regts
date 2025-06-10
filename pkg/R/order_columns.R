#' Order the columns of a matrix, data frame or timeseries alphabetically.
#'
#' This function orders the columns of a matrix, data frame or
#' multivariate timeseries alphabetically.
#'
#' @param x An R object with column names (a `matrix`, `ts`, `regts`
#' or `data.frame`).
#' @return The same object as `data`, but with alphabetically sorted columns.
#' @examples
#' # multivariate timeseries
#' ts1 <- regts(matrix(1:9, ncol = 3), start = "2010Q4",
#'              names = c("c", "b", "a"))
#' print(order_columns(ts1))

#' @seealso \code{\link{rename_cols}}
#' @importFrom data.table is.data.table
#' @export
order_columns <- function(x) {
  cnames <- colnames(x)
  if (is.null(cnames)) {
    stop("Argument 'x' does not have column names")
  }
  if (is.data.table(x)) {
    return(x[, order(cnames), with = FALSE])
  } else {
    return(x[, order(cnames), drop = FALSE])
  }
}
