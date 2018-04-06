#' Calculate the differences between two multivariate timeseries
#' objects
#'
#' This function can be used to compare two multivariate timeseries objects.
#' The result is a list with a \code{\link{regts}} component with the computed
#' differences or \code{NULL} if there are no differences.
#' The function returns a list with the differences, the names of columns with
#' differences larger than a specified tolerance, and the names of the columns
#' present in one object but missing in the other object. The return value also
#' includes differences in the period ranges.
#'
#' @details
#' This function calculates the difference between common columns
#' of two multivariate timeseries objects \code{x1} and \code{x2}.
#' The two timeseries must have the same frequency, but may have a different
#' period range. The difference is computed for the intersection of the two
#' period ranges. Two \code{NA} or two \code{NaN} values are considered to be
#' equal. A \code{NA} value is not equal to a \code{NaN} value.
#' The function also returns missing column names in one of the two objects,
#'
#' @export
#' @param x1 the first timeseries (a multivariate \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param x2 the second timeseries (a multivariate \code{regts} or \code{ts} object).
#' @param tol difference tolerance (by default zero). Differences with absolute
#' values smaller than or equal to \code{tol} are ignored.
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
#'  Only timeseries with differences larger than \code{tol} are included.
#'  Leading and trailing rows with differences less than \code{tol} have also been
#'  removed.}
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

  series_name1 <- deparse(substitute(x1))
  series_name2 <- deparse(substitute(x2))

  if (!is.mts(x1)) {
    stop(paste0("Argument x1 (", series_name1,
                ") is not a multivariate timeseries"))
  }
  if (!is.mts(x2)) {
    stop(paste0("Argument x2 (", series_name2,
                ") is not a multivariate timeseries"))
  }

  if (tol < 0) {
    stop("Argument tol should be >= 0")
  }

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

  # check if results are equal
  equal <- ranges_equal && length(missing_names1) == 0 &&
           length(missing_names2) == 0 && length(difnames) == 0

  ret <- list(equal          = equal,
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

  # If xx1 and xx2 are both NA or NaN, then replace NA or NaN with 0.
  # Two NA or two NaN values are always considered equal.
  both_nan <- is.nan(xx1) & is.nan(xx2)
  xx1[both_nan] <- 0
  xx2[both_nan] <- 0
  # is.na(x) is TRUE for NA  AND  NaN !!
  both_na <- is.na(xx1) & !is.nan(xx1) & is.na(xx2) & !is.nan(xx2)
  xx1[both_na] <- 0
  xx2[both_na] <- 0

  dif <- fun(xx1, xx2)
  colnames(dif) <- common_names

  sel <- apply(abs(dif), FUN = max, MARGIN = 2) > tol
  sel[is.na(sel)] <- TRUE
  if (any(sel)) {

    # remove columns with differences <= tol
    dif <- dif[, sel, drop = FALSE]

    # now remove leading/trailing rows with differences <= tol
    row_sel <- apply(abs(dif), FUN = max, MARGIN = 1) > tol
    row_sel[is.na(row_sel)] <- TRUE
    row_sel <- which(row_sel)
    row_sel <- min(row_sel) : max(row_sel)
    pstart <- start_period(common_range) + row_sel[1] - 1
    period <- period_range(pstart, pstart + length(row_sel) - 1)
    dif <- dif[period, , drop = FALSE]

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

  max_names <- 50
  nmax <- 10

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

        # print first max_names difnames with largest difference
        cat("Names of timeseries with differences (alphabetical)\n")
        if (length(difnames) > max_names){
          print(difnames[1:max_names])
          cat("[ reached max print -- omitted ",length(difnames) - max_names,
              "names ]\n")
        } else {
          print(difnames)
        }

        nmax <- min(ncol(dif), nmax)

        # absolute values to get largest values per timeseries with function max
        absdif <- abs(dif)
        maxdif <- as.numeric(apply(absdif, FUN = max, MARGIN = 2))
        # print only nmax values
        maxdif <- maxdif[1:nmax]

        # reorder maxdif, with Na's first
        # maxdif can contain Na values, we want to print them first, followed
        # by names with highest differences
        index <- order(maxdif, decreasing = TRUE, na.last = FALSE)

        # find position of values, if NA search for position NA
        pos_func <- function(i){
          mdif <- maxdif[i]
          pos <- Position(f = function(x) {if (is.na(mdif)) is.na(x) else x == mdif},
                          absdif[,i])
          return(pos)
        }
        pos <- sapply(index, FUN = pos_func)

        startp <- start_period(dif)
        periods <- sapply(pos, FUN = function(i) {as.character(startp + i - 1)})

        idx_names <- difnames[index]

        # periods and idx_names are already in index order
        max_value <- function(i){
          v <- dif[periods[i], idx_names[i]]
          return(as.numeric(v))
        }
        max_values <- sapply(1:nmax, FUN = max_value)

        df <- data.frame(Period = periods, Max_dif = max_values)
        rownames(df) <- idx_names

        cat("\nTimeseries with largest differences\n\n")
        # print first nmax difnames with largest difference in specified period
        print(df)

        if (ncol(dif) > nmax){
          cat("[ reached max print -- omitted ",ncol(dif) - nmax, "names ]\n")
        }

        # print timeseries with largest difference
        cat(sprintf("\nTimeseries with largest difference (%s)\n", difnames[index[1]]))
        print(dif[, difnames[index[1]]])

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
        if (length(missing_names1) > max_names){
          cat(paste("Missing timeseries in ", ts_names[1]), ":\n")
          print(missing_names1[1:max_names])
          cat("[ reached max print -- omitted ",length(missing_names1) - max_names,
              "names ]\n")
        } else {
          cat(paste("Missing timeseries in ", ts_names[1]), ":\n")
          print(missing_names1)
        }
      }
      cat("\n")
      if (length(missing_names2) > 0) {
        if (length(missing_names2) > max_names){
          cat(paste("Missing timeseries in ", ts_names[2]), ":\n")
          print(missing_names2[1:max_names])
          cat("[ reached max print -- omitted ",length(missing_names2) - max_names,
              "names ]\n")
        } else {
          cat(paste("Missing timeseries in ", ts_names[2]), ":\n")
          print(missing_names1)
        }
      }

      if (!ranges_equal) {
        cat("\nThe two timeseries have different period ranges:\n")
        df <- data.frame(ranges = c(as.character(period_range1),
                                    as.character(period_range2)))
        rownames(df) <- ts_names
        print(df)
      }
    }
  })
}
