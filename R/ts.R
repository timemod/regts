#' Convert a \link{regts} to a normal \link{ts}
#'
#' @param x a \code{regts}
#' @return a \code{ts}
#' @export
as.ts.regts <- function(x) {
    class(x) <- class(x)[-1]
    return (x)
}
