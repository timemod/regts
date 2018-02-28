#' Convert a \code{\link{regts}} to a \code{\link[base]{data.frame}}
#'
#' @details
#' If the \code{regts} has labels,
#' then the labels are added to columns of the data frame using the function
#' \code{\link[Hmisc]{label}} of package \code{Hmisc}
#' if argument \code{rowwise} is \code{FALSE}.

#' @param x a \code{\link{regts}}
#' @param rowwise a logical value: should the timeseries be stored rowwise
#' or columnwise in the data frame? Defaults to \code{FALSE}
#' @param row_names Whether to create row names. If \code{FALSE},
#' then an additional column with name \code{"period"} or \code{"name"} is created for
#' columnwise or rowwise timeseries, respectively.
#' @param ... additional arguments to be passed to methods.
#' @return A \code{\link[base]{data.frame}}
#' @export
#' @examples
#' ts <- regts(matrix(1:4, ncol = 2) , start = "2015Q3", names = c("a", "b"),
#'            labels = c("Timeseries a", "Timeseries b"))
#' print(as.data.frame(ts))
as.data.frame.regts <- function(x, rowwise = FALSE, row_names = TRUE, ...) {

  if (!is.matrix(x)) {
    x <- univec2unimat(x, deparse(substitute(x)))
  }

  # convert the time index to a character vector with period texts
  first_period <- start_period.ts(x)
  times <- sapply(0 : (NROW(x) - 1),
                  FUN = function(i) as.character(first_period + i))

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
      Hmisc::label(ret, self = FALSE) <- lbls
    }

    if (row_names) {
      rownames(ret) <- times
    } else {
      ret <- cbind(period = times, ret, stringsAsFactors = FALSE)
    }



  }
  return(ret)
}
