#' Function for removing leading and trailing NAs
#'
#' This function removes leading or trailing NAs or both from a (multivariate)
#' regts object.
#' For multivariate regts a row will by default be regarded as NA if all elements
#' in the row are NA.  Specify \code{"any"} to change this behaviour.
#'
#' @param x a regts object
#' @param method character with values \code{"both",} \code{"first"} or
#' \code{"last"} to remove NAs at both ends (by default), just at the start
#' or just at the end.
#' @param is_na character with values \code{"all"} or \code{"any"}. If \code{"all"}
#' (default) then a row will be regarded as \code{NA} only if all elements in
#' the row are \code{NA}. If \code{"any"} then a row will be regarded as \code{NA}
#' if it has any \code{NA}s. For one dimensional regts objects this argument
#' has no effect.
#' @return A \code{regts} object in which leading and/or trailing NAs have been removed.
#'
#' @examples
#' # remove only leading NAs
#' ts1 <- regts(c(NA,1,3,NA,4,8,NA), start = "2000")
#' na_trim(ts1, method = "first")
#'
#' # remove trailing NAs
#' data <- matrix(c(1,3,NA,2,5,NA,3,7,NA), ncol = 3)
#' rts <- regts(data, start = "2010Q2", names = c("a", "b", "c"))
#' na_trim(rts, method = "last")
#'
#' data <- matrix(c(NA,3,NA,NA,5,6,NA,7,NA), ncol = 3)
#' rts <- regts(data, start = "2010Q1", names = c("a", "b", "c"))
#' # remove leading NAs if all elements in the row are NA
#' na_trim(rts, method = "first")
#' # or remove rows if any NA occurs in that row
#' na_trim(rts, is_na = "any")

#' @export
na_trim <- function (x, method = c("both", "first","last"),
                     is_na = c("all", "any")) {

    if (!is.ts(x)) {
        stop("Argument x is not a timeseries")
    }
    side <- match.arg(method)
    isna <- match.arg(is_na)

    if (!is.matrix(x)) {
        elem <- which(!is.na(x))
        len <- length(x)
    } else {
        if (isna == "all") {
            elem <-   which(!apply(is.na(x), 1, all))
        } else {
            elem <-   which(!apply(is.na(x), 1, any))
        }
        len <- nrow(x)
    }
    if (length(elem) == 0) {
        return(NULL)
    }
    if (side == "both") {
        sel <- min(elem) : max(elem)
    } else if (side == "first") {
        sel <- min(elem) : len
    } else {
        sel <- 1 : max(elem)
    }
    per <- start_period(get_regperiod_range(x)) + sel[1] - 1
    period <- regperiod_range(per, per + length(sel) - 1)

    return(x[period,])

}
