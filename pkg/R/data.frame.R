#' Convert a \code{\link{regts}} to a \code{\link[base]{data.frame}}
#'
#' @details
#' If the \code{regts} has labels and argument \code{rowwise} is \code{FALSE},
#' then the labels are added to columns of the data frame
#' These labels are visible in the data viewer.

#' @param x a \code{\link{regts}}
#' @param rowwise a logical value: should the timeseries be stored rowwise
#' or columnwise in the data frame? Defaults to \code{FALSE}
#' @param row_names Whether to create row names. If \code{FALSE},
#' then an additional column with name \code{"period"} or \code{"name"} is
#' created for columnwise or rowwise timeseries, respectively.
#' @param period_as_date A logical (default \code{FALSE}).
#' If \code{TRUE} the periods are stored as \code{\link[base]{Date}} objects.
#' Depending on arguments \code{rowwise} and \code{row_names}
#' the periods may appear in the row or column names of the result data frame.
#' In that case the dates are coerced to character vectors,
#' using the standard date format \code{"\%Y-\%m-\%d"}
#' (see the documentation of function \code{\link[base]{strptime}}
#' for more information about date formats).
#' @param ... additional arguments to be passed to methods.
#' @return A \code{\link[base]{data.frame}}
#' @name as.data.frame
#' @export
#' @examples
#' ts <- regts(matrix(1:4, ncol = 2) , start = "2015Q3", names = c("a", "b"),
#'            labels = c("Timeseries a", "Timeseries b"))
#' print(as.data.frame(ts))
as.data.frame.regts <- function(x, ..., rowwise = FALSE, row_names = TRUE,
                                period_as_date = FALSE) {

  if (!is.matrix(x)) {
    xsub <- substitute(x)
    # when as.data.frame.regts is called by View, xsub has class
    # regts, in that case deparse does not give the desired result
    name <- if (is.regts(xsub)) "Series" else deparse(xsub)
    x <- univec2unimat(x, name)
  }

  # convert the time index to a character vector with period texts
  first_period <- start_period.ts(x)
  if (NROW(x) == 1) {
    periods <- list(first_period)
  } else {
    periods <- first_period + (0 : (NROW(x) - 1))
  }

  if (period_as_date)  {
    times <- lapply(periods, FUN = as.Date)
    # convert list of Dates to a vector of Dates. sapply doesn't work here
    times <- do.call(c, times)
  } else {
    times <- sapply(periods, FUN = as.character)
  }

  lbls <- ts_labels(x)

  if (rowwise) {

    ret <- as.data.frame(t(x), ...)
    colnames(ret) <- times

    if (!is.null(lbls)) {
      ret <- cbind(label = lbls, ret, stringsAsFactors = FALSE)
    }

    if (!row_names) {
      rownames(ret) <- NULL
      ret <- cbind(name = colnames(x), ret, stringsAsFactors = FALSE)
    }

  } else {

    ret <- as.data.frame.ts(x, ...)

    # add labels
    if (!is.null(lbls)) {
      # use as.character to convert a named character vector to a normal
      # character vector
      ret <- set_labels_df(ret, as.character(lbls))
    }

    if (row_names) {
      rownames(ret) <- times
    } else {
      ret <- cbind(period = times, ret, stringsAsFactors = FALSE)
    }



  }
  return(ret)
}
