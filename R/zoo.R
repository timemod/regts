#' @export
as.regts.zoo <- function(x, ...) {
    return (as.regts(as.ts(x)))
}

#' @importFrom zoo as.zooreg
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

#' @importFrom zoo as.zoo
#' @export
as.zoo.regts <- function(x, ...) {
    return (zoo::as.zooreg(x, ...))
}
