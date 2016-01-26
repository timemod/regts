#' Conversion between dataframes and regts.
#'
#' Zoo has excellent facilities for this,
#' therefore we employ zoo for the time being.

#' Convert a regts to a dataframe.
#' For the time being use zoo as intermediate class.
#' @export
#' @import zoo
as.data.frame.regts <- function(x, ...) {
    return (as.data.frame(as.zooreg(x)))
}

#' @export
# For the time being use zoo as intermediate class
as.regts.data.frame <- function(x, ...) {
    x <- read.zoo(x, ...)
    return (as.regts(x))
}
