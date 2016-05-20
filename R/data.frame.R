#' Converts a \link{regts} to a \link{data.frame}
#'
#' The \code{regts} is first converted to a \link{zoo} object using
#' \link{as.zooreg} and then converted to a data frame with \link{as.data.frame.zoo}
#' @param x a \code{regts}
#' @param ... arguments passed to \link{as.data.frame.zoo}
#' @return A \code{data.frame}
#' @import zoo
#' @export
#' @examples
#' ts <- regts(1:3 , start = "2015Q3", names = "a")
#' print(as.data.frame(ts))
as.data.frame.regts <- function(x, ...) {
    return (as.data.frame(as.zooreg(x), ...))
}


