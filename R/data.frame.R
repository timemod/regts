#' Converts a \link{regts} to a \link{data.frame}
#'
#' @details
#' The \code{regts} is first converted to a \link{zoo} object using
#' \link{as.zooreg} and then converted to a data frame with \link{as.data.frame.zoo}.
#' If the \code{regts} has labels, then the labels are added to the result dataframe using
#' package \link{Hmisc}.
#' @param x a \code{regts}
#' @param ... arguments passed to \link{as.data.frame.zoo}
#' @return A \code{data.frame}
#' @import zoo
#' @import Hmisc
#' @export
#' @examples
#' ts <- regts(matrix(1:4, ncol = 2) , start = "2015Q3", names = c("a", "b"),
#'            labels = c("Timeseries a", "Timeseries b"))
#' print(as.data.frame(ts))
as.data.frame.regts <- function(x, ...) {
    ret <- as.data.frame(as.zooreg(x), ...)
    lbls <- ts_labels(x)
    if (!is.null(lbls)) {
        label(ret, self = FALSE) <- lbls
    }
    return(ret)
}
