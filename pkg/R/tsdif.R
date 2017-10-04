#' Calculate the differences between two multivariate timeseries
#' objects
#'
#' This function can be used to compare two multivariate timeseries objects.
#' The result is a list with a \code{\link{regts}} component with the computed
#' differences or \code{NULL} if there are no differences.
#' The function reports the names of columns with differences larger than a
#' specified tolerance, and the names of the columns present in one object
#' but missing in the other object.  The function also reports differences
#' in the period ranges.
#'
#' @details
#' This function calculates the difference between common columns
#' of two multivariate timeseries objects \code{x1} and \code{x2}.
#' The two timeseries must have the same frequency, but may have a different
#' period range. The difference is computed for the intersection of the two
#' period ranges. Two \code{NA} values are considered to be equal.
#' The function reports missing column names in one of the two objects,
#'
#' @export
#' @param x1 the first timeseries (a multivariate \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param x2 the second timeseries (a multivariate \code{regts} or \code{ts} object).
#' @param tol difference tolerance (by default zero). Differences with absolute
#' values smaller than tol are ignored.
#' @param fun function to compute differences. This function should accept
#' two arguments (two numbers) for which the difference is computed.
#' By default the normal difference (\eqn{x_1 - x_2}) is computed. A useful function for
#' computing differences is \code{\link{cvgdif}}, which computes relative differences
#' if the absolute value of \code{x2} is larger than 1.
#' @return a list with class "tsdif", with the following components
#'  \item{equal}{\code{TRUE} if \code{x1} and \code{x2} have the same column names
#'               and period ranges,
#'              and if all differences are smaller than or equal to \code{tol}}
#'  \item{difnames}{The names of the timeseries with differences
#'                   larger than \code{tol}}
#'  \item{dif}{A \code{regts} with the computed differences,
#'  or \code{NULL} if there are no differences larger than \code{tol}.
#'  Only timeseries with differences larger than \code{tol} are included.}
#'  \item{common_names}{the names of the common columns}
#'  \item{missing_names1}{The names of columns present in \code{x2} but missing
#'                        in \code{x1}}
#'  \item{missing_names2}{The names of columns present in \code{x1} but missing
#'                        in \code{x2}}
#'  \item{period_range1}{The period ranges of \code{x1} as a
#'  \code{\link{period_range}} object}
#'  \item{period_range2}{The period ranges of \code{x2} as a
#'  \code{period_range} object}
#'  \item{common_range}{The intersection of the period ranges }
#'  \item{ranges_equal}{A logical indicating whether the period ranges of
#'  \code{x1} and \code{x2} differ}
#'  \item{ts_names}{a character string giving the names of the two input
#'  timeseries}
#'  \item{tol}{The tolerance parameter}
#'  \item{fun}{a character string specifying the supplied function \code{fun},
#'  or \code{NULL} if \code{fun} has not been specified}
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
#' @seealso
#'\code{\link{regts}}
#'
tsdif <- function(x1, x2, tol = 0, fun = function(x1, x2) (x1 - x2)) {

  if (!is.mts(x1)) {
      stop(paste0("Argument x1 (", deparse(substitute(x1))),
                   ") is not a multivariate timeseries")
  }
  if (!is.mts(x2)) {
      stop(paste0("Argument x2 (", deparse(substitute(x2))),
           ") is not a multivariate timeseries")
  }

  series_name1 <- deparse(substitute(x1))
  series_name2 <- deparse(substitute(x2))

  if (frequency(x1) != frequency(x2)) {
      stop(paste0("Timeseries x1 and x2 (", series_name1, " and ", series_name2,
                 ") have different frequencies"))
  }

  x1 <- as.regts(x1)
  x2 <- as.regts(x2)

  names1 <- colnames(x1)
  names2 <- colnames(x2)

  # create colnames if x1 or x2 do not have colnames
  if (is.null(names1)) {
      names1 <- paste("column", 1 : ncol(x1))
      colnames(x1) <- names1
  }
  if (is.null(names2)) {
      names2 <- paste("column", 1 : ncol(x2))
      colnames(x2) <- names2
  }

  common_names   <- intersect(names1, names2)
  missing_names1 <- setdiff(names2, names1)
  missing_names2 <- setdiff(names1, names2)

  period_range1  <- get_period_range(x1)
  period_range2  <- get_period_range(x2)
  common_range   <- range_intersect(period_range1, period_range2)
  ranges_equal   <- period_range1 == period_range2

  if (length(common_names) == 0) {
    warning(paste("Timeseries", series_name1, "and", series_name2,
                  "have no common columns"))
  }
  if (is.null(common_range)) {
    warning(paste("Timeseries", series_name1, "and", series_name2,
                  "have no common period range"))
  }
  if (anyDuplicated(names1)) {
    stop(paste("Duplicate column names in timeseries", series_name1))
  }
  if (anyDuplicated(names2)) {
    stop(paste("Duplicate column names in timeseries", series_name2))
  }

  dif <- calculate_difference(common_names, common_range, x1, x2, tol, fun)

  if (!is.null(dif)) {
      difnames <- colnames(dif)
  } else {
      difnames <- character(0)
  }

  ret <- list(equal = length(missing_names1) == 0 &&
                length(missing_names2) == 0 &&
                length(difnames) == 0 &&
                ranges_equal,
              difnames       = difnames,
              dif            = dif,
              common_names   = common_names,
              missing_names1 = missing_names1,
              missing_names2 = missing_names2,
              common_range   = common_range,
              period_range1  = period_range1,
              period_range2  = period_range2,
              ranges_equal   = ranges_equal,
              ts_names       = c(series_name1, series_name2),
              tol            = tol,
              fun            = if (missing(fun)) {
                                 NULL
                               } else {
                                 deparse(substitute(fun))
                               })

  return(structure(ret, class = "tsdif"))
}

# Calculate the difference for the common columns in x1 and x2,
# and return a regts with the difference. Return NULL if the differences
# are smaller than tol, or if the two timeseries have no common columns
calculate_difference <- function(common_names, common_range, x1, x2, tol, fun) {

  var_count <- length(common_names)
  if (var_count == 0 || is.null(common_range) || nperiod(common_range) == 0) {
      return (NULL)
  }

  xx1 <- x1[common_range, common_names, drop = FALSE]
  xx2 <- x2[common_range, common_names, drop = FALSE]

  # If xx1 and xx2 are both NA, then replace NA with 0.
  # Two NA values are always considered equal.
  both_na <- is.na(xx1) & is.na(xx2)
  xx1[both_na] <- 0
  xx2[both_na] <- 0

  dif <- fun(xx1, xx2)
  colnames(dif) <- common_names

  sel <- apply(abs(dif), FUN = max, MARGIN = 2) > tol
  sel[is.na(sel)] <- TRUE
  if (any(sel)) {
      dif <- dif[, sel, drop = FALSE]
  } else {
      dif <- NULL
  }

  if (!is.null(dif)) {
      # sort columns of dif
      dif <- dif[, sort(colnames(dif)), drop = FALSE]
  }
}

#' Calculate the 'convergence difference'
#'
#' \code{cvgdif} calculates the difference  between two numeric vectors
#' \eqn{x_1} and \eqn{x_2} according to \eqn{|x_1 - x_2|/\max(|x_2|, 1)}.
#' This difference is equivalent to the convergence test employed in the
#' package \code{isismdl}.
#' @param x1 first numeric vector
#' @param x2 second numeric vector
#' @return the 'convergence difference' as described above
#'
#'@seealso
#'\code{\link{tsdif}}
#'
#' @examples
#'# create two timeseries x1 and x2
#'x1 <- regts(matrix(data = rep(1:27), nc = 3), start = "2008Q4",
#'            names = c("a", "b", "c"))
#'x2 <- x1 + 0.001
#'colnames(x2) <- c("a", "b", "d")
#'
#'# calculate the differences
#'cvgdif(x1, x2)
#'
#' @export
cvgdif <- function(x1, x2) {
  x_abs = abs(x2)
  dif <- abs(x1 - x2) / ifelse(x_abs < 1, 1, x_abs)
  return(dif)
}

#' @export
print.tsdif <- function(x, ...) {
  cat("\n     tsdif timeseries comparison result\n\n")
  with(x, {
    if (equal  && is.null(fun) && tol == 0) {
      cat(paste("The two timeseries objects", ts_names[1], "and", ts_names[2],
                "are equal!\n"))
    } else {
      cat(paste("Compared timeseries:", ts_names[1], "and", ts_names[2], "\n"))
      if (!is.null(fun)) {
        cat("Difference function:", fun, "\n")
      }
      if (tol != 0) {
        cat("Difference tolerance:", tol, "\n")
      }
      cat("\n")
      if (!is.null(dif)) {
        cat("Differences\n\n")
        print(dif)
        cat("Names of timeseries with differences:\n")
        print(difnames)

      } else {
        if (is.null(common_range)) {
          cat(paste0("No differences computed because the two timeseries\n",
                    "have no overlapping period ranges\n"))
        } else if (length(common_names) == 0) {
          cat(paste("No differences computed because the two timeseries",
                    "have no common columns\n"))
        } else if (tol == 0) {
          cat("No differences found\n")
        } else {
          cat(paste("No differences larger than", tol, "found\n"))
        }
      }
      cat("\n")
      if (length(missing_names1) > 0) {
        cat(paste("Missing timeseries in ", ts_names[1]), ":\n")
        print(missing_names1)
      }
      if (length(missing_names2) > 0) {
        cat(paste("Missing timeseries in ", ts_names[2]), ":\n")
        print(missing_names2)
      }
      if (!ranges_equal) {
        cat("The two timeseries have different period ranges:\n")
        df <- data.frame(ranges = c(as.character(period_range1),
                                    as.character(period_range2)))
        rownames(df) <- ts_names
        print(df)
      }
    }
  })
}
