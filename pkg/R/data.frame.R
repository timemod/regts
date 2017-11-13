#' Convert a \code{\link{regts}} to a \code{\link[base]{data.frame}}
#'
#' @details
#' Unlike \code{\link{as.regts.ts}} this function adds row names with a period
#' indicator.
#' If the \code{regts} has labels, then the labels are added to the result
#' dataframe using the function \code{\link[Hmisc]{label}}
#' of package \code{Hmisc}.

#' @param x a \code{\link{regts}}
#' @param ... additional arguments to be passed to methods.
#' @return A \code{\link[base]{data.frame}}
#' @export
#' @examples
#' ts <- regts(matrix(1:4, ncol = 2) , start = "2015Q3", names = c("a", "b"),
#'            labels = c("Timeseries a", "Timeseries b"))
#' print(as.data.frame(ts))
as.data.frame.regts <- function(x, ...) {

  if (!is.matrix(x)) {
    x <- univec2unimat(x, deparse(substitute(x)))
  }

  # convert the time index to a character vector with period texts
  first_period <- start_period.ts(x)
  times <- sapply(0 : (NROW(x) - 1),
                  FUN = function(i) as.character(first_period + i))

  ret <- as.data.frame.ts(x, row.names = times, ...)



  # handle labels
  lbls <- ts_labels(x)
  if (!is.null(lbls)) {
    Hmisc::label(ret, self = FALSE) <- lbls
  }
  return(ret)
}
