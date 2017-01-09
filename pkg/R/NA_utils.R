#' Function for removing leading and trailing NAs
#'
#' This function removes leading or trailing Nas or both from a (multivariate)
#' regts object.
#' For multivariate regts a row will default be regarded as NA if all elements
#' in the row are NA.  Specify \code{"any"} to change this behaviour.
#'
#' @param x a regts object
#' @param sides character with values \code{"both",} \code{"left"} or
#' \code{"right"} to remove NAs from
#' both sides (the default), just from the left side or just from the right side.
#' @param is.na character with values \code{"all"} or \code{"any"}. If \code{"all"}
#' (default) then a row will be regarded as \code{NA} only if all elements in
#' the row are \code{NA}. If \code{"any"} then a row will be regarded as \code{NA}
#' if it has any \code{NA}s. For one dimensional regts objects this argument
#' has no effect.
#' @return A \code{regts} object in which leading and/or trailing NAs have been removed.
#'
#' @examples
#' # remove only leading NAs
#' ts1 <- regts(c(NA,1,3,NA,4,8,NA), start = "2000")
#' na.trim(ts1, sides = "left")
#'
#' # remove trailing NAs if all elements in the row are NA
#' data <- matrix(c(1,3,NA,2,5,NA,3,7,NA), ncol = 3)
#' rts <- regts(data, start = "2010Q2", names = c("a", "b", "c"))
#' na.trim(rts, sides = "right")
#'
#' data <- matrix(c(NA,3,NA,NA,5,6,NA,7,NA), ncol = 3)
#' rts <- regts(data, start = "2010Q1", names = c("a", "b", "c"))
#' # remove leading/trailing NAs if all elements in the row are NA
#' na.trim(rts)
#' # or remove NAs if any NA occurs in that row
#' na.trim(rts, is.na = "any")

#' @export
na_skip <- function (x, sides = c("both", "left","right"),
                     is.na = c("all", "any")) {

    if (!is.ts(x)) {
        stop("Argument x is not a timeseries")
    }
    side <- match.arg(sides)
    isna <- match.arg(is.na)

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
    if (side == "both") {
        sel <- min(elem) : max(elem)
    } else if (side == "left") {
        sel <- min(elem) : len
    } else {
        sel <- 1 : max(elem)
    }
    per <- start_period(get_regperiod_range(x)) + sel[1] - 1
    if (!is.matrix(x)) {
        return(regts(x[sel], start = per))
    } else {
        return(regts(x[sel, ], start = per))
    }
}


