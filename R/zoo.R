#' @export
#' @import zoo
as.regts.zoo <- function(x, ...) {
    return (as.regts(as.ts(x)))
}

#' @export
as.zooreg.regts <- function(x, ...) {
    f <- frequency(x)
    x <- NextMethod("as.zooreg")
    if (f == 4) {
        index(x) <- zoo::as.yearqtr(index(x))
    } else if (f == 12) {
        index(x) <- zoo::as.yearmon(index(x))
    }
    return (x)
}

#' @export
as.zoo.regts <- function(x, ...) {
    return (as.zooreg(x, ...))
}
