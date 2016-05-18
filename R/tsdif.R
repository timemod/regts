#' Calculates the difference between two multivariate ts objects
#'
#'This function calculates the difference between two multivariate timeseries
#'objects \code{x1} and \code{x2} for the common columns.
#'The two timeseries must have the same frequency, but may have a different
#'period ranges. The difference is computed for the intersection of the two
#'period ranges (for the non-overlapping periods the result will be
#'\code{NA}). Two \code{NA} values are considered to be equal.
#'
#' @export
#' @param x1 the first timeseries (a \link{regts} or \link{mts} object).
#' @param x2 the second timeseries (a \link{regts} or \link{mts} object).
#' @param tol difference tolerance (by default zero). Differences smaller
#' than tol are ignored.
#' @param fun function to compute differences. This function should accept
#' two arguments (two numbers) for which the difference is computed.
#' By default the absolute difference is computed. A useful function for
#' computing difference is \link{cvgdif}, which computes relative differences
#' if the absolute value of \code{x2} is larger than 1.
#' @return a list with the following components
#'  \item{tol}{The tolerance parameter}
#'  \item{missing_names1}{The names of columns present in \code{x2} but missing
#'                        in \code{x1}}
#'  \item{missing_names2}{The names of columns present in \code{x1} but missing
#'                        in \code{x2}}
#'  \item{common_names}{The names of the common columns of \code{x1} and
#'                     \code{x2}}
#'  \item{has_dif}{\code{TRUE} if any difference larger than \code{tol} was
#'                 found}
#'  \item{difnames}{The names of the common columns with differences
#'                   larger than \code{tol}}
#'  \item{dif}{The computed differences for the common columns with
#'             differences are larger than \code{tol}, or \code{NULL} is there
#'             are no differences}
#' @examples
#' library(regts)
#'
#'# create two timeseries x1 and x2
#'x1 <- regts(matrix(data = rep(1:27), nc = 3), start = "2008Q4",
#'            names = c("a", "b", "c"))
#'x2 <- x1 + 0.001
#'colnames(x2) <- c("a", "b", "d")
#'
#'dif1 <- tsdif(x1, x2)
#'print(dif1)
#'
#'# use the function regts::cvgdif (convergence difference)
#'dif2 <- tsdif(x1, x2, fun = cvgdif)
#'print(dif2)
#'
#'# ignore differences smaller than 1e-4
#'dif3 <- tsdif(x1, x2, tol = 1e-4, fun = cvgdif)
#'print(dif3)
#'
#'
tsdif <- function(x1, x2, tol = 0, fun = function(x1, x2) abs(x1 - x2)) {

    seriesName1 <- deparse(substitute(x1))
    seriesName2 <- deparse(substitute(x2))

    # tsdif currently only works if both x1 and x2 are multivariate timeseries
    if (!inherits(x1, "mts")) {
        stop(paste("Argument x1 (", seriesName1, "is not a multivariate ts"))
    }
    if (!inherits(x2, "mts")) {
        stop(paste("Argument x2 (", seriesName2, "is not a multivariate ts"))
    }

    if (frequency(x1) != frequency(x2)) {
        stop(paste("Timeseries x1 and x2 (", seriesName1, "and", seriesName2,
                   "have different frequencies"))
    }

    names1 <- colnames(x1)
    names2 <- colnames(x2)

    common_names   <- intersect(names1, names2)
    missing_names1 <- setdiff(names2, names1)
    missing_names2 <- setdiff(names1, names2)

    varCount <- length(common_names)

    xx1 <- x1[, common_names, drop = FALSE]
    xx2 <- x2[, common_names, drop = FALSE]

    # make sure that xx1 and xx2 have the same time axis
    # TODO: is there not a more efficient way to this?
    # calculate the intersection of period, then adjust each period
    tot <- regts.intersect(xx1, xx2)
    xx1 <- tot[, 1: varCount, drop = FALSE]
    xx2 <- tot[, (varCount + 1) : (2 * varCount), drop = FALSE]
    rm(tot)

    # If xx1 and xx2 are both NA, then replace NA with 0.
    # Two NA values are always considered equal.
    both_na <- is.na(xx1) & is.na(xx1)
    xx1[both_na] <- 0
    xx2[both_na] <- 0

    dif <- fun(xx1, xx2)
    colnames(dif) <- common_names

    sel <- apply(dif, FUN = max, MARGIN = 2) > tol
    sel[is.na(sel)] <- TRUE
    if (any(sel)) {
        dif <- dif[, sel, drop = FALSE]
    } else {
        dif <- NULL
    }

    if (!is.null(dif)) {
        # sort columns of dif
        difnames <- colnames(dif)
        dif <- dif[, sort(difnames), drop = FALSE]
    } else {
        difnames <- character(0)
    }

    retval <- list(tol           = tol,
                   missing_names1 = missing_names1,
                   missing_names2 = missing_names2,
                   common_names   = common_names,
                   has_dif       = length(difnames) > 0,
                   difnames      = difnames,
                   dif           = dif)

    return (retval)
}

#' Calculates the 'convergence difference'
#'
#' \code{cvgdif} calculates the difference  between two numbers
#' \code{x1} and \code{x2} according to \code{|x_1 - x_2| / max(|x_2|, 1)}.
#' This difference is equivalent to the convergence test employed in the
#' package \link{macromod}.
#' @param x1 first number
#' @param x2 second number
#' @return the 'convergence difference' as described above
#' @export
cvgdif <- function(x1, x2) {
    x_abs = abs(x2)
    dif <- abs(x1 - x2) / ifelse(x_abs < 1, 1, x_abs)
    return(dif)
}
