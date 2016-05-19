#' @export
lag.regts <- function(x, ...) {
    x <- remove_regts_class(x)
    return (as.regts(NextMethod(.Generic)))
}

#' @export
diff.regts <- function(x, ...) {
    x <- remove_regts_class(x)
    return (as.regts(NextMethod(.Generic)))
}

#' @export
window.regts <- function(x, ...) {
    # convert x to normal ts object
    x <- remove_regts_class(x)
    return (as.regts(NextMethod(.Generic)))
}

#' @export
regts.union <- function(...) {
    return (as.regts(ts.union(...)))
}

#' @export
regts.intersect <- function(...) {
    return (as.regts(ts.intersect(...)))
}


#' cbind
#'
#' @export
 cbind.regts <- function(...) {
    # we cannot simply use .Generic, because the method dispatching is not done via UserMethod(),
    # but by C-internal patching. Therefore use the following tric:
    arguments <- list(...)
    names(arguments) <- eval(substitute(alist(...)))

    # convert regts object to normal objects
    f <- function(x) {
       if (is.regts(x)) {
           return (remove_regts_class(x))
       } else {
            return (x)
       }
     }
     arguments <- lapply(timeseries, FUN = f)

     return (as.regts(do.call(cbind, timeseries)))
}

#' @export
Ops.regts <- function(x, y) {
    return (as.regts(NextMethod(.Generic)))
}
