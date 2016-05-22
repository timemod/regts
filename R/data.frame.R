#' Converts a \link{regts} to a \link{data.frame}
#'
#'The timeseries can be put either as columns or as rows in the data frame. The
#'advantage of a rowwise storage is that an additional column with
#'timeseries labels can be inserted in the data frame.
#'
#' @details
#' The \code{regts} is first converted to a \link{zoo} object using
#' \link{as.zooreg} and then converted to a data frame with \link{as.data.frame.zoo}
#' @param x a \code{regts}
#' @param columnwise a logical value. Specify \code{TRUE} if the timeseries
#' should become the  columns of the data frame, and \code{FALSE} is the timeseries
#' should become the rows of the data frame. In that case, the first column
#' of the result data frame will contain the labels.
#' @param ... arguments passed to \link{as.data.frame.zoo}
#' @return A \code{data.frame}
#' @import zoo
#' @export
#' @examples
#' ts <- regts(matrix(1:4, ncol = 2) , start = "2015Q3", names = c("a", "b"),
#'            labels = c("Timeseries a", "Timeseries b"))
#' print(as.data.frame(ts))
#'
#' # store timeseries rowwise. The first column contains the labels.
#' print(as.data.frame(ts, columnwise = FALSE))
as.data.frame.regts <- function(x, columnwise = TRUE, ...) {
    ret <- as.data.frame(as.zooreg(x), ...)
    if (!columnwise) {
        ret <- as.data.frame(t(ret))
        lbls <- ts_labels(x)
        if (!is.null(lbls)) {
            ret <- cbind(data.frame(labels = ts_labels(x)), ret)
        }
    }
    return(ret)
}
