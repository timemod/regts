#' Create a \code{regts} timeseries object
#'
#' The \code{regts} class is an extension of the \code{\link[stats]{ts}}
#' class of the \link{stats} package. Working with \code{regts} makes it
#' easier to select periods.
#'
#' @param data a vector or matrix of the observed timeseries values.
#' A \code{\link[base]{data.frame}}
#' will be coerced to a numeric matrix via \code{\link{data.matrix}}.
#' (See also the description of the
#' function \code{\link[stats]{ts}} of the \code{\link{stats}} package).
#' @param start the starting period as a  \code{\link{period}} object or a
#' character string that can be converted to a \code{period} object.
#' If not specified, then the start period is calculated
#' from argument \code{end} and the dimension of \code{data}.
#' @param end the end period as a  \code{\link{period}} object or a character
#' string that can be converted to a \code{period} object. If not
#' specified, then the end period is calculated from argument \code{start} and
#' the dimension of \code{data}.
#' @param period the period range as a \code{\link{period_range}} object or a
#' character string that can be converted to a \code{period_range} object.
#' This argument replaces arguments \code{start} and \code{end}.
#' @param frequency the frequency of the timeseries. This argument should only be
#' specified if the \code{start}, \code{end} or \code{period} argument is
#' specified with a general period format without period indicator,
#' e.g. \code{"2011-3"}.

#' @param names a character vector with the column names for the series
#' if \code{data} is a matrix or data frame. Defaults to the column names of data.
#' @param labels a character vector of labels (descriptions of the timeseries)
#' @return a \code{regts} object
#' @examples
#' # univariate timeseries
#' ts1 <- regts(1:10, start = "2010Q4")
#'
#' # period selection
#' print(ts1["2011Q2/2011Q3"])
#'
#' # multivariate timeseries
#' ts2 <- regts(matrix(1:9, ncol = 3), start = "2010Q4", names = c("a", "b", "c"))
#'
#' # two equivalent ways to select a column in a multivariate ts
#' print(ts2$a)
#' print(ts2[, "a"])
#'
#' # period selection in multivariate regts
#' print(ts2["2011Q2/2011Q3"])
#'
#' # period and column selection in multivariate regts
#' print(ts2["2011Q2/2011Q3", "a"])
#'
#' # two equivalent ways to add a column
#' ts2$d <- 2
#' ts2[ , "e"] <- 2
#'
#' # multivariate timeseries with labels
#' ts3 <- regts(matrix(1:9, ncol = 3), start = "2010Q4", names = c("a", "b", "c"),
#'              labels = paste("Timeseries", c("a", "b", "c")))
#'
#'# multivariate timeseries created with a period_range object
#' range <- period_range("2016Q1", "2017Q4")
#' ts4 <- regts(matrix(1:16, ncol = 2), period = range, names = c("a", "b"))
#'
#' # create a half-yearly timeseries; because argument end is specified the
#' # length of the timeseries is smaller than the length of data (10).
#' ts5 <- regts(1:10, start = "2010-1", end = '2011-2', frequency = 2)
#'
#' @seealso
#' The function \code{\link{is.regts}} can be used to test if an object is a
#' \code{regts}.
#'
#' The S3 generic \code{\link{as.regts}} can be used to coerce an R object to a
#'  \code{regts}. There are currently methods for \code{\link[stats]{ts}} and
#'  \code{\link[base]{data.frame}}.
#'
#' \code{\link{as.data.frame}} and \code{\link{as.list}} can be used
#' to convert \code{regts} to a \code{\link[base]{data.frame}} or a
#' \code{\link[base]{list}}.
#'
#' Function \code{\link{cbind}} can be used to bind two or more
#' timeseries objects and create a multivariate \code{regts}.
#'
#' Information about the time period of the timeseries can be obtained
#' with the functions \code{\link{get_period_range}},
#' \code{\link{start_period}} and \code{\link{end_period}}.
#'
#' See also the description of the functions for handling labels
#' (\code{\link{ts_labels}} and \code{\link{update_ts_labels}}).
#'
#' @importFrom stats ts
#' @importFrom stats frequency
#' @export
regts <- function(data, start, end, period, frequency = NA,
                  names = colnames(data), labels = NULL) {

  # Check the periodrange
  if (!missing(period) && !missing(start)) {
    stop("Arguments 'start' and 'period' exclude each other!")
  }
  if (missing(start) && missing(end) && missing(period)) {
    start <- create_period(1, 1)
  }

  if (!missing(period)) {
    if (!is.period_range(period)) {
      period <- as.period_range(period)
    }
    start <- start_period(period)
    end <- end_period(period)
    # start and end cannot both be NULL
    if(!is.null(start)){
      freq <- frequency(start)
      if(is.null(end)){
        end <- start + NROW(data) - 1
      }
    } else {
      freq <- frequency(end)
      start <- end - NROW(data) + 1
    }
  } else {
    if (!missing(start)) {
      if (length(start) > 1) stop("Argument 'start' must be of length 1")
      start <- as.period(start, frequency)
      freq <- frequency(start)
    }
    if (!missing(end)) {
      if (length(end) > 1) stop("Argument 'end' must be of length 1")
      end <- as.period(end, frequency)
      if (missing(start)) {
        freq <- frequency(end)
      } else if (frequency(end) != freq) {
        stop("'start' and 'end' have different frequencies")
      }
    }
    if (missing(end)) {
      end <- start + NROW(data) - 1
    } else if (missing(start)){
      start <- end - NROW(data) + 1
    }
  }

  if (NROW(data) == 0) {
    stop("`regts` object must have one or more observations")
  }

  # from now on, work with numerical values (the number of subperiod after
  # Christ), this is more efficient.
  start <- as.numeric(start)
  end   <- as.numeric(end)

  if (start > end) {
    stop("'start' cannot be after 'end'")
  }

  # CONVERT DATA
  if (is.data.frame(data)) {
    data <- data.matrix(data)
  }
  if (is.matrix(data)) {
    ndata <- nrow(data)
    if (!is.null(names) && length(names) != ncol(data)) {
      stop(paste("The length of the names vector is not equal to",
                 "the number of columns"))
    }
    dimnames(data) <- list(NULL, names)

  } else {
    ndata <- length(data)
    if (!missing(names) && !is.null(names)) {
      warning("Argument names is ignored if data is a vector")
    }
  }
  if (ndata == 0) {
    stop("'ts' object must have one or more observations")
  }
  nobs <- end - start + 1
  if (nobs != ndata) {
    data <- if (is.matrix(data)) {
      if (ndata < nobs)
        data[rep_len(1L:ndata, nobs),  , drop = FALSE]
      else if (ndata > nobs)
        data[1L:nobs, , drop = FALSE]
    } else {
      if (ndata < nobs) {
        rep_len(data, nobs)
      } else if (ndata > nobs) {
        data[1L:nobs]
      }
    }
  }

  return (create_regts(data, start, end, freq, labels))
}

# Internal function to create a regts. No checking of input data.
# startp and endp are the number of subperiods (e.g. quarters) after
# Christ, freq is the frequency of the timeseries.
create_regts <- function(data, startp, endp, freq, labels) {
  attr(data, "tsp") <- c(startp / freq, endp / freq, freq)
  class(data) <- if (NCOL(data) > 1) {
    c("regts", "mts", "ts", "matrix")
  }  else {
    c("regts", "ts")
  }
  if (!is.null(labels)) {
    ts_labels(data) <- labels
  }
  return (data)
}

#' Test whether an object is a \code{\link{regts}} timeseries object
#' @param x any R object.
#' @return \code{TRUE} if \code{x} is a \code{regts}
#' @seealso
#' \code{\link{regts}} and  \code{\link{as.regts}}
#' @examples
#' a <- regts(1:15, start = "2011Q2")
#' is.regts(a)
#' @export
is.regts <- function(x) {
  return (inherits(x, "regts"))
}

#' Coerce an object to a \code{\link{regts}} timeseries object
#'
#' @param x an arbitrary R object.
#' @param time_column the column names or numbers of the data frame
#' in which the time (periods) is stored. Specify \code{0} if the index is in
#' the row names of the data frame.
#' If \code{time_column} has length > 1, then argument \code{fun} should be
#' a function which converts a data frame to \code{period} vector.
#' @param numeric logical: should non numeric values be converted to numeric data.
#' By default they are converted to numeric. This can be changed by setting
#' \code{numeric = FALSE}.
#' @param fun a function for converting values in the row names or
#' time column(s) to \code{\link{period}} objects. Normally this is a function
#' which converts a vector to a \code{period} vector (for example
#' function \code{period}). See argument \code{time_column} for exceptions.
#' @param strict A logical. If \code{TRUE} (the default) all periods between the
#' start and the end period must be present.
#' Otherwise the timeseries are filled with \code{NA} for the missing periods.
#' @param ... arguments passed to \code{fun}.
#' @return a \code{regts} object
#' @seealso \code{\link{regts}}, \code{\link{is.regts}},
#' \code{\link{as.data.frame}},
#' \code{\link{as.list}}, \code{\link{start_period}}, \code{\link{end_period}}
#' @examples
#' # convert a ts to regts
#' x <- ts(1:3, start = c(2015,3), frequency = 4)
#' x <- as.regts(x)
#'
#' # Now three examples for converting a data.frame
#'
#' # create a data frame with timeseries and with the
#' # time index in the rownames, and convert to a regts
#' df <- data.frame(a = 1:3)
#' rownames(df) <- c("2015Q3", "2015Q4", "2016Q1")
#' ts <- as.regts(df)
#'
#' # create a data frame with the time index in the first column and special
#' # time format "2015 3" instead of "2015Q3", and convert to regts
#' df <- data.frame(periods = c("2015 3", "2015 4", "2016 1"),  a = 1:3)
#' ts <- as.regts(df, time_column = 1, frequency = 4)
#'
#' # create a data frame with non numeric data and convert to regts
#' # Strings containing non numeric values are converted to NA
#' # Logical values TRUE/FALSE are converted to 1/0
#' df <- data.frame(a = c("1", "2", "X"), b
#' = c(TRUE, FALSE, TRUE), stringsAsFactors = FALSE)
#' as.regts(df)
#'
#' # data frame with the years in the first column and quarters in the
#' # second column
#' df <- data.frame(years = c(2018, 2018), quarters = c(1, 2), a = 1:2)
#' fun <- function(x) {period(paste(x[[1]], x[[2]]), frequency = 4)}
#' as.regts(df, time_column = c("years", "quarters"), fun = fun)
#'
#' @export
as.regts <- function(x, ...) {
  UseMethod("as.regts")
}

#' @export
as.regts.regts <- function(x, ...) {
  return (x)
}

#' @describeIn as.regts Coerce a \code{\link[stats]{ts}} to a
#' \code{regts}
#' @export
as.regts.ts <- function(x, ...) {
  class(x) <- c("regts", class(x))
  return (x)
}

#' @export
as.ts.regts <- function(x, ...) {
  classes <- class(x)
  class(x) <- classes[classes != "regts"]
  return(x)
}

#' @describeIn as.regts Convert a \code{\link[base]{data.frame}} to a
#' \code{regts}. The time should be stored in the row numbers
#' of the matrix
#' @export
as.regts.data.frame <- function(x, time_column = 0, numeric = TRUE,
                                fun = period, strict = TRUE, ...) {

  # extract time column(s) and data from the input data frame,
  # and convert to a matrix. data are converted to numeric if necessary

  # x could be a subclass of a data frame, for example a data.table or tibble.
  # Therefore use function as.data.frame to make sure that x is a normal
  # data frame.

  x <- as.data.frame(x)

  if (nrow(x) == 0) {
    stop("'regts' object must have one or more observations")
  }

  if (length(time_column) == 1 && time_column == 0) {
    periods <- rownames(x)
    data <- x
  } else {
    if (is.character(time_column)) {
      time_column <- which(colnames(x) %in% time_column)
    }
    periods <- x[ , time_column]
    data <- x[-time_column]
  }

  if (numeric) {
    datamat <- numeric_matrix(data)
  } else {
    datamat <- as.matrix(data)
  }

  periods <- convert_periods(periods, fun = fun, ...)
  if (is.list(periods)) {
    stop("The time column(s) contain different frequencies")
  }

  # Use numeric == FALSE, because the data has already been converted
  # to numeric when needed
  ret <- matrix2regts_(datamat, periods, numeric = FALSE, strict = strict)

  # handle labels
  if (ncol(data) > 0) {
    lbls <- get_labels_df(data)
    if (!all(nchar(lbls, type = "bytes") == 0)) {
      ts_labels(ret) <- lbls
    }
  }

  return (ret)
}

#' @describeIn as.regts Convert a \code{\link{matrix}} to a
#' \code{regts}
#' @export
as.regts.matrix <- function(x, numeric = TRUE, fun = period, strict = TRUE,
                            ...) {
  if (nrow(x) == 0) {
    stop("'regts' object must have one or more observations")
  }

  periods <- rownames(x)
  if (is.null(periods)) {
    periods <- period(seq_len(nrow(x)), frequency = 1)
  } else {
    periods <- convert_periods(periods, fun = fun, ...)
    if (is.list(periods)) {
      stop("The row names contain periods with different frquencies")
    }
  }

  # we assume that the periods are in the rownames of the matrix
  return(matrix2regts_(x, periods, numeric = numeric, strict = strict))
}


# numeric_matrix is an internal function to convert a data.frame to a numeric
# matrix.
#
# It returns the matrix obtained by converting all the variables in a data frame
# to numeric mode and then binding them together as the columns of a matrix.
# Factors, Dates and other non-numerical variables are converted to characters.
#
# This function is similar to the function data.matrix of the base package,
# except that:
# * it is possible to specify a decimal separator used to convert strings to
#   numeric values
# * factors are first converted to characters and then to numeric values
# * if the data frame contains strings that cannot be converted
#   to numerical values, then numeric_matrix gives a warning about
#   the first 10 character strings that could not be succesfully converted.
#
# INPUT
#   x    a data frame
#   dec  decimal separator
numeric_matrix <- function(x, dec = ".") {

  if (nrow(x) == 0 || ncol(x) == 0) {
    # no data available
    return(matrix(0.0, nrow = nrow(x), ncol = ncol(x)))
  }

  x <- as.data.frame(x)

  # Convert factors, Dates etc. to characters. Also replace
  # decimals separator with ".".
  convert_col <- function(x) {
    if (is.numeric(x) || is.logical(x)) {
      return(x)
    } else {
      x <- as.character(x)
      x[trimws(x) == ""] <- NA_character_
      if (dec != ".") {
        return(sub(dec, ".", x, fixed = TRUE))
      } else {
        return(x)
      }
    }
  }

  x_converted <- as.data.frame(lapply(x, FUN = convert_col),
                               stringsAsFactors = FALSE, optional = TRUE)

  num_mat <- suppressWarnings(data.matrix(x_converted))

  error_sel <- is.na(num_mat) & !is.na(x_converted)

  if (any(error_sel)) {
    weird_texts <- unique(x[error_sel])
    nweird <- length(weird_texts)
    NWEIRD_MAX <- 10
    nmax <- min(NWEIRD_MAX, nweird)
    weird_texts <- paste0("\"", weird_texts[1:nmax], "\"")

    if (nweird <= NWEIRD_MAX) {
      warning(paste0("NAs introduced by coercion.\n",
                     "The following texts could not be converted to numeric:\n",
                     paste0(weird_texts, collapse = "\n")))
    } else {
      warning(paste0("NAs introduced by coercion.\n",
                     nweird, " texts could not be converted to numeric.\n",
                     "The first ", NWEIRD_MAX, " texts that gave problems are:\n",
                     paste0(weird_texts, collapse = "\n")))
    }
  }

  return(num_mat)
}

# Function convert_periods: Internal function that converts a vector or
# data frame to a period vector. Optionally, a coercion function fun can be
# specified. If the periods have different frequencies it returns a list with
# periods.
convert_periods <- function(periods, fun = period, ...) {

  # finally convert to a list of period objects
  # create a list of regpriod objects
  periods <- fun(periods, ...)

  if (is.period(periods)) {
    return(periods)
  } else if (is.list(periods) && all(sapply(periods, FUN = is.period))) {
    freqs <- unique(sapply(periods, FUN = frequency))
    if (length(freqs) == 1) {
      # all periods have the same frequency -> convert to vector
      warning(paste("Function 'fun' returns a list of period objects instead",
                    "of a period vector.\nThe list is converted to a vector"))
      return(create_period(unlist(periods), frequency = freqs))
    } else {
      # different frequencies -> give error message in calling function.
      return(periods)
    }
  }

  stop("Function 'fun' should return a period vector")
}

# matrix2regts_ : internal function to convert a matrix to a regts.
# INPUT:  x        a matrix. If x has column names, then these become the
#                  names of the timeseries.
#         periods  a period vector
#         numeric  TRUE if x should be converted to numeric.
#         strict   strict parameter (see documentation of as.regts).
# RETURN: a regts object
matrix2regts_ <- function(x, periods, numeric, strict) {

  if (numeric && !is.numeric(x)) {
    x <- as.numeric(x)
  }

  freq <- frequency(periods)

  if (anyDuplicated(periods)) {
    dupl <- duplicated(periods)
    period_strings <- unique(as.character(periods[dupl]))
    stop(paste0("Duplicate periods found in data (",
               paste(period_strings, collapse = ", "), ")."))
  }

  sorted_periods <- sort(periods[1]:periods[nrow(x)])
  if (identical(periods, sorted_periods)) {
    # normal regular timeseries, no missing periods and periods
    # are ordered synchronically
    ret <- regts(x, start = periods[1], freq)
  } else {
    # irregular timeseries in data frame (missing periods or unordered time index.
    # stop if strict and missing periods
    if (strict){
      dif <- setdiff(sorted_periods, periods)
      if (length(dif) > 0) {
        dif_periods <- create_period(dif, freq)
        missing_periods <- as.character(dif_periods)
        mp <- paste(missing_periods, collapse = ", ")
        stop(paste0("Missing periods found (", mp, "). Set parameter strict to FALSE!"))
      }
    }

    p_min <- min(periods)
    p_max <- max(periods)
    per_count <- p_max - p_min + 1
    mat <- matrix(NA, nrow = per_count, ncol = ncol(x))
    colnames(mat) <- colnames(x)
    ret <- regts(mat, start = p_min)
    rows <- periods - p_min + 1
    ret[rows, ] <- x
  }
  return (ret)
}


#' @describeIn as.regts Default method to convert an R object to a
#' \code{regts}. This method first employs \code{\link[stats:ts]{as.ts}}
#' and then \code{\link{as.regts.ts}}
#' @importFrom stats as.ts
#' @export
as.regts.default <- function(x, ...) {
  return (as.regts(as.ts(x, ...)))
}

# Add columns with names new_colnames to x, and fill with NA
#' @importFrom stats ts.union
add_columns <- function(x, new_colnames) {
  ncols <- length(new_colnames)
  new_columns <- matrix(NA, nrow = nrow(x), ncol = ncols)
  ret <- as.regts(ts.union(x, new_columns))
  colnames(ret) <- c(colnames(x), new_colnames)
  lbls <- attr(x, "ts_labels")
  if (!is.null(lbls)) {
    ts_labels(ret) <- c(lbls, rep("", ncols))
  }
  return (ret)
}

# Selection on the left-hand side: replace a part of a regts
# (e.g. x["2010Q2", ] <- 2).
#' @importFrom stats is.mts
#' @export
"[<-.regts" <- function (x, i, j, value) {

  if (is.null(value) && is.matrix(x)) {
    # remove columns
    if (!missing(i)) {
      stop("Row selection not allowed when the replacement is NULL")
    }
    if (missing(j)) {
      # x[] <- NULL: remove all columns
      return(x[, numeric(0)])
    } else {
      if (length(j) == 0) {
        return(x)
      }
      if (is.numeric(j)) {
        return(x[, -j, drop = FALSE])
      } else if (is.logical(j)) {
        return(x[, !j, drop = FALSE])
      } else {
        colsel <- match(as.character(j), colnames(x))
        colsel <- colsel[!is.na(colsel)]
        if (length(colsel) > 0) {
          return(x[, -colsel, drop = FALSE])
        } else {
          # no matching columns, do nothing
          return(x)
        }
      }
    }
  }

  if (!missing(j) && is.character(j)) {
    # Check if j contains names of columns not present in x.
    # Add missing columns if necessary
    cnames <- colnames(x)
    if (is.null(cnames)) {
      stop("object has no column names")
    }
    new_colnames <- setdiff(j, cnames)
    if (length(new_colnames) > 0) {
      x <- add_columns(x, new_colnames)
    }
  }

  if (!missing(i) && (is.character(i) || inherits(i, "period") ||
                      inherits(i, "period_range"))) {

    # call C++ function get_period_range
    ts_range <- get_period_range(x)

    sel_range <- convert_selection_range(as.period_range(i), ts_range)
    if (sel_range[1] < ts_range[1] || sel_range[2] > ts_range[2]) {
      ts_range <- c(min(sel_range[1], ts_range[1]),
                    max(sel_range[2], ts_range[2]), ts_range[3])
      x <- window_regts(x, ts_range)
    }
    i <- seq(sel_range[1] - ts_range[1] + 1,
             length.out = nperiod__(sel_range))
    # if argument j is missing, then we have to add an empty
    # column selection. x[i] does not return the same as x[i, ].
    if (missing(j) && is.mts(x)) {
      x[i, ] <- value
      return (x)
    }
  }
  return (NextMethod("[<-"))
}

# Selection on the right-hand-side (e.g. x["2010Q2", ]).
#' @importFrom stats is.ts
#' @export
"[.regts" <- function(x, i, j, drop = TRUE) {

  j_missing <- missing(j)

  # save function call for error handling
  func_call <- sys.call()

  tryCatch({
    if (missing(i)) {
      lbls <- ts_labels(x)
      if (!is.null(lbls) && !missing(j)) {
        lbls <- lbls[j]
      }
      if (is.matrix(x) && nrow(x) == 1 && (missing(j) || length(j) > 1)) {
        # the result is very weird is the timeseries has a single row
        # and if drop = TRUE is used
        ret <- NextMethod(.Generic, drop = FALSE)
      } else {
        ret <- NextMethod(.Generic)
      }
      ret <- as.regts(ret)
      if (!is.null(lbls)) {
        ts_labels(ret) <- lbls
      }
      return(ret)
    } else {
      # row selection present
      if (is.character(i) || inherits(i, "period") ||
          inherits(i, "period_range")) {
        # first select columns
        if (!missing(j)) {
          x <- x[ , j, drop = drop]
        }
        # the row selector is a period_range. Use window_regts
        return(window_regts(x, as.period_range(i)))
      } else  {
        # numeric / logical row selection: the result is a  matrix or vector
        # (no longer a ts)
        return(NextMethod(.Generic))
      }
    }
  }, warning = function(w) {
    warning(w)
  }, error = function(err) {
    if (!j_missing && is.character(j) && err$message == "subscript out of bounds") {
      missing_cols <- setdiff(j, colnames(x))
      message <- paste0("Undefined columns: ",
                       paste(missing_cols, collapse = ", "), ".")
      message_lines <- strwrap(message, width = 80)
      message <- paste(message_lines, collapse = "\n")
      stop(simpleError(message, call = func_call))
    } else {
      stop(err)
    }
  })
}

# This function converts period_range object sel_range so that it is a valid
# period selector for a timeseries with period range ts_range. This implies
# that the frequency is made equal to the frequency of ts_range and that
# NA values are replaced by the values of ts_range.
convert_selection_range <- function(sel_range, ts_range) {

  # convert frequency
  if (ts_range[3] %% sel_range[3] != 0) {
    stop(paste0("frequency of timeseries (", ts_range[3],
                ") not divisible by the frequency of",
                "the selector (", sel_range[3], ")."))
  }
  fac <- ts_range[3] %/% sel_range[3]
  new_sel_range <- numeric(3)
  new_sel_range[1] <- floor(sel_range[1] * fac)
  new_sel_range[2] <- floor((sel_range[2] + 1) * fac - 1)
  new_sel_range[3] <- ts_range[3]

  # replace NA values by values from ts_range
  new_sel_range <- ifelse(is.na(new_sel_range), ts_range, new_sel_range)

  if (new_sel_range[2] < new_sel_range[1]) {
    # ERROR: the start period is after the end period.
    if (is.na(sel_range[1])) {
      pstart <- start_period(ts_range)
      pend <- end_period(sel_range)
    } else {
      pstart <- start_period(ts_range)
      pend <- end_period(ts_range)
    }
    stop(paste0("The start period (", pstart, ") is after the end period (",
               pend, ")."))
  }
  return (new_sel_range)
}

window_regts <- function(x, sel_range) {
  ts_range <- get_period_range(x)
  sel_range <- convert_selection_range(sel_range, ts_range)
  nper_new <- nperiod__(sel_range)
  if (nper_new < 0) {
    stop("Illegal selection")
  }
  shift <- sel_range[1] - ts_range[1]
  rmin <- max(1, 1 - shift)
  rmax <- min(nper_new, nperiod__(ts_range) - shift)

  # Create NA with the correct type, this is necessary when the period
  # selection lies completely outside the period range of the timeseries.
  if (is.integer(x)) {
    na_val <- NA_integer_
  } else if (is.numeric(x)) {
    na_val <- NA_real_
  } else if (is.character(x)) {
    na_val <- NA_character_
  } else {
    na_val <- NA
  }

  if (is.matrix(x)) {
    data <- matrix(na_val, nrow = nper_new, ncol = ncol(x))
    if (rmax >= rmin) {
      data[rmin:rmax, ] <- x[(rmin+shift):(rmax+shift), ]
    }
    colnames(data) <- colnames(x)
  } else {
    data <- logical(nper_new)
    data[] <- na_val
    if (rmax >= rmin) {
      data[rmin:rmax] <- x[(rmin+shift):(rmax+shift)]
    }
  }
  return(create_regts(data, sel_range[1], sel_range[2], sel_range[3],
                      ts_labels(x)))
}

#' @export
"$.regts" <- function(object, x) {
  if (!is.matrix(object)) stop("$ operator not possible for vector timeseries")
  cnames <- colnames(object)
  if (is.null(cnames)) stop(paste("$ operator not possible for regts without",
                            "column names"))
  i <- match(x, cnames)
  if (is.na(i)) {
    return(NULL)
  } else {
    return(object[, i])
  }
}

#' @export
"$<-.regts" <- function(object, x, value) {
  if (!is.matrix(object)) stop("$ operator not possible for vector timeseries")
  cnames <- colnames(object)
  if (is.null(cnames)) stop(paste("$ operator not possible for regts without",
                                  "column names"))

  if (is.null(value)) {
    i <- pmatch(x, cnames)
    if (!is.na(i)) {
      object <- object[, -i, drop = FALSE]
    }
  } else {
    object[, x] <- value
  }

  return(object)
}

