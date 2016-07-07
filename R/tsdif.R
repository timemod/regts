#' Calculates the difference between two timeseries objects
#'
#'This function calculates the difference between two timeseries
#'objects \code{x1} and \code{x2} for the common columns.
#'The two timeseries must have the same frequency, but may have a different
#'period range. The difference is computed for the intersection of the two
#'period ranges (for the non-overlapping periods the result will be
#'\code{NA}). Two \code{NA} values are considered to be equal.
#'
#' @export
#' @param x1 the first timeseries (a \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param x2 the second timeseries (a \code{regts} or \code{ts} object).
#' @param tol difference tolerance (by default zero). Differences smaller
#' than tol are ignored.
#' @param fun function to compute differences. This function should accept
#' two arguments (two numbers) for which the difference is computed.
#' By default the absolute difference is computed. A useful function for
#' computing difference is \code{\link{cvgdif}}, which computes relative differences
#' if the absolute value of \code{x2} is larger than 1.
#' @return a list with the following components
#'  \item{tol}{The tolerance parameter}
#'  \item{missing_names1}{The names of columns present in \code{x2} but missing
#'                        in \code{x1}}
#'  \item{missing_names2}{The names of columns present in \code{x1} but missing
#'                        in \code{x2}}
#'  \item{common_names}{The names of the common columns of \code{x1} and
#'                     \code{x2}}
#'  \item{equal}{\code{TRUE} if \code{x1} and \code{x2} have the same column names
#'              and if all differences are smaller than or equal to \code{tol}}
#'  \item{difnames}{The names of the common columns with differences
#'                   larger than \code{tol}}
#'  \item{dif}{A \code{\link{regts}} with the computed differences for the common columns with
#'             differences larger than \code{tol}, or \code{NULL} if there
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
#'# calculate and print the differences
#'dif1 <- tsdif(x1, x2)
#'print(dif1)
#'
#'# use the function cvgdif (convergence difference)
#'dif2 <- tsdif(x1, x2, fun = cvgdif)
#'
#'# calculate differences with tol = 1e-4, and print the
#'# names of the timeseries with differences larger than tol
#'dif3 <- tsdif(x1, x2, tol = 1e-4, fun = cvgdif)
#'print(dif3$difnames)
#'
#'@seealso
#'\code{\link{regts}}
#'
tsdif <- function(x1, x2, tol = 0, fun = function(x1, x2) abs(x1 - x2)) {

    seriesName1 <- deparse(substitute(x1))
    seriesName2 <- deparse(substitute(x2))

    x1 <- as.regts(x1)
    x2 <- as.regts(x2)

    if (frequency(x1) != frequency(x2)) {
        stop(paste("Timeseries x1 and x2 (", seriesName1, "and", seriesName2,
                   "have different frequencies"))
    }

    names1 <- colnames(x1)
    names2 <- colnames(x2)

    common_names   <- intersect(names1, names2)
    missing_names1 <- setdiff(names2, names1)
    missing_names2 <- setdiff(names1, names2)

    dif <- calculate_difference(common_names, x1, x2, tol, fun)
    if (!is.null(dif)) {
        difnames <- colnames(dif)
    } else {
        difnames <- character(0)
    }

    retval <- list(tol           = tol,
                   missing_names1 = missing_names1,
                   missing_names2 = missing_names2,
                   common_names   = common_names,
                   equal          = length(missing_names1) == 0 & length(missing_names2) == 0
                                    & length(difnames) == 0,
                   difnames       = difnames,
                   dif            = dif)

    return (retval)
}

# Calculate the difference for the common columns in x1 and x2,
# and return a regts with the difference. Return NULL if the differences
# are smaller than tol, or if the two timeseries have no common columns
calculate_difference <- function(common_names, x1, x2, tol, fun) {

    var_count <- length(common_names)
    if (var_count == 0) {
        return (NULL)
    }

    xx1 <- x1[, common_names]
    xx2 <- x2[, common_names]

    # Make sure that xx1 and xx2 have the same time axis
    # TODO: is there not a more efficient way to this?
    # Calculate the union of period, then adjust each period.
    tot <- regts.union(xx1, xx2)
    xx1 <- tot[, 1: var_count]
    xx2 <- tot[, (var_count + 1) : (2 * var_count)]
    rm(tot)

    # If xx1 and xx2 are both NA, then replace NA with 0.
    # Two NA values are always considered equal.
    both_na <- is.na(xx1) & is.na(xx2)
    xx1[both_na] <- 0
    xx2[both_na] <- 0

    dif <- fun(xx1, xx2)
    colnames(dif) <- common_names

    sel <- apply(dif, FUN = max, MARGIN = 2) > tol
    sel[is.na(sel)] <- TRUE
    if (any(sel)) {
        dif <- dif[, sel]
    } else {
        dif <- NULL
    }

    if (!is.null(dif)) {
        # sort columns of dif
        dif <- dif[, sort(colnames(dif))]
    }
}

#' Calculates the 'convergence difference'
#'
#' \code{cvgdif} calculates the difference  between two numbers
#' \eqn{x_1} and \eqn{x_2} according to \eqn{\frac{|x_1 - x_2|}{\max(|x_2|, 1)}}.
#' This difference is equivalent to the convergence test employed in the
#' package \code{macromod}.
#' @param x1 first number
#' @param x2 second number
#' @return the 'convergence difference' as described above
#' @export
cvgdif <- function(x1, x2) {
    x_abs = abs(x2)
    dif <- abs(x1 - x2) / ifelse(x_abs < 1, 1, x_abs)
    return(dif)
}
