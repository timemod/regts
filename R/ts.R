#' Convert a \link{regts} to a normal \link{ts}
#'
#' @param x a \code{regts}
#' @param ... arguments passed to methods (unused for the default method).
#' @return a \code{ts}
#' @importFrom stats as.ts
#' @export
as.ts.regts <- function(x, ...) {
    class(x) <- class(x)[-1]
    return (x)
}
