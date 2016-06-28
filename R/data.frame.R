#' Converts a \link{regts} to a \link{data.frame}
#'
#' @details
#' If the \code{regts} has labels, then the labels are added to the result
#' dataframe using package \link{Hmisc}.
#' @param x a \code{regts}
#' @return A \code{data.frame}
#' @export
#' @examples
#' ts <- regts(matrix(1:4, ncol = 2) , start = "2015Q3", names = c("a", "b"),
#'            labels = c("Timeseries a", "Timeseries b"))
#' print(as.data.frame(ts))
as.data.frame.regts <- function(x) {

    # convert the time index to a character vector with period texts
    first_period <- start_period.ts(x)
    times <- unlist(lapply(0 : (nrow(x) - 1),
                           FUN = function(x) as.character(first_period + x)))

    # convert ts to data.frame (using as.data.frame.ts), and set rownames
    ret <- NextMethod(.Generic)
    rownames(ret) <- times

    # handle labels
    lbls <- ts_labels(x)
    if (!is.null(lbls)) {
        Hmisc::label(ret, self = FALSE) <- lbls
    }
    return(ret)
}
