#' @export
lag.regts <- function(x, ...) {
    return (as.regts(NextMethod(.Generic)))
}

#' @export
diff.regts <- function(x, ...) {
    return (as.regts(NextMethod(.Generic)))
}

#' @export
window.regts <- function(x, ...) {
    return (as.regts(NextMethod(.Generic)))
}

#' @export
regts.union <- function(...) {
    return (as.regts(ts.union(...)))
}

#' @export
regts.intersect <- function(...) {
    return (as.regts(ts.intersection(...)))
}

#' @export
Ops.regts <- function(x, y) {
    return (as.regts(NextMethod(.Generic)))
}
