#' Create a \code{\link{period_range}} object.
#'
#' A \code{period_range} object represents an interval of periods,
#' for example a period from \code{"2017Q2"} to \code{"2019Q3"}.
#' Function \code{period_range}  creates a \code{period_range} from
#' a single character string (e.g. \code{"2017Q2/2019Q3"}, see Details)
#' or from two R objects that can be coerced to \code{period} objects.
#' \cr\cr
#' Function \code{as.period_range} coerces an R object to a \code{period_range}
#' if possible.
#'
#' It is possible to create a \code{period_range} from
#' a single string specifying a period range, for example
#' \code{"2017Q2/2019Q3"}. For this format, the first and last
#' period are separated by \code{"/"}. The first and last period
#' are specified according to the same format recognized by function
#' \code{\link{period}}. The first or last period may be omitted
#' (e.g. \code{"2017Q3/"})), in that case the period range
#' has no lower or upper bound. The string format is case insensitive.
#'
#' @param start the first period (a \code{period}, an object that can be coerced
#' to a \code{period}, or by default \code{NULL}). If \code{start} is \code{NULL}
#' the lower bound of the period range is undetermined. \code{start} can also
#' be a character string specifying a period range, for example
#' \code{"2010Q2/2011Q3"}).
#' @param end the last period (a \code{period}, an object that can be coerced
#' to a \code{period}, or by default \code{NULL}).
#' If \code{end} is \code{NULL}, the upper bound of the period range is undetermined.
#' @param x an R object
#' @param frequency frequency of the period objects. This argument is mandatory
#' if argument \code{start} or \code{end} is a character with general period format
#' without frequency indicator (e.g. \code{"2011-1"})
#' @param ... additional arguments to be passed to or from methods (currently
#' not used in package \code{regts})
#' @return a \code{period_range} object
#' @examples
#' # two methods to create a period_range from 2010Q2 to 2016Q3
#' period_range("2010Q2", "2016Q3")
#' period_range("2010Q2/2016Q3")
#'
#' # create a period_range for the first 5 quarters after 2013Q2
#' start <- period("2013q3")
#' period_range(start, start + 5)
#'
#' # create a period_range from 2010Q2 with no upper bound
#' period_range("2010q2", NULL)
#'
#' # create a period_range for a timeseries with frequency 2 (half year)
#' period_range("2010-2", "2016-2", frequency = 2)
#'
#' # convert a period object to a period_range with equal start and end period
#' p <- period("2010Q2")
#' as.period_range(p, p)
#'
#' # create a month range starting at the month 1000 days before
#' # the current day and ending at the current month.
#' today <- Sys.Date()
#' period_range(today - 1000, today)
#' @seealso \code{\link{period}}, \code{\link{nperiod}},
#' \code{\link{start_period}} and \code{\link{end_period}}
#' @export
period_range <- function(start = NULL, end = NULL, frequency = NA) {

  if (is.null(start) && is.null(end)) {
    stop("At least one of the periods should not be NULL")
  }

  if (!is.null(start)) {
    if (is.character(start) && grepl("/", start[1])) {
      # direct conversion, inputs "2016q1" and "2016q1/2017q1" are possible
      if (is.null(end)){
        return(as.period_range(start, frequency))
      }
      else {
        stop("Argument end should not be specified if start is a period range string")
      }
    }
    else{
      start <- as.period(start, frequency)
      freq1 <- attr(start, 'frequency')
    }
  }
  if (!is.null(end)) {
    end <- as.period(end, frequency)
    freq2 <- attr(end, 'frequency')
  }
  if ((!(is.null(start) || is.null(end)))) {
    if (freq1 != freq2) {
      stop("The two periods have different frequency")
    }
    if (end < start) {
      stop(paste0("The start period (", start, ") is after the end period (", end, ")"))
    }
  }
  if (!is.null(start)) {
    freq <- freq1
  } else {
    freq <- freq2
  }

  # convert periods to normal numbers
  if (!is.null(start)) {
    start <- as.numeric(start)
  } else {
    start <- NA_real_
  }
  if (!is.null(end)) {
    end <- as.numeric(end)
  } else {
    end <- NA_real_
  }
  return(create_period_range(start, end, freq))
}

#' @rdname period_range
#' @export
as.period_range <- function(x, frequency = NA, ...) {
  UseMethod("as.period_range")
}

#' @export
as.period_range.character <- function(x, frequency = NA, ...) {
  if (length(x) != 1) {
    stop("x should be a single character string")
  }
  return(parse_period_range(x, frequency))
}

#' @export
as.period_range.period_range <- function(x, ...) {
  return(x)
}

#' @export
as.period_range.period <- function(x, ...) {
  return(create_period_range(as.numeric(x), as.numeric(x), frequency(x)))
}

#' @export
as.period_range.numeric <- function(x, frequency = NA, ...) {
  start <- as.period(x, frequency, ...)
  return(as.period_range(start))
}

#' @export
as.period_range.POSIXlt <- function(x, frequency = NA, ...) {
  return(period_range(as.period(x, frequency), frequency = frequency))
}

#' @export
as.period_range.POSIXct <- function(x, frequency = NA, ...) {
  return(period_range(as.period(x, frequency), frequency = frequency))
}

#' @export
as.period_range.Date <- function(x, frequency = NA, ...) {
  return(period_range(as.period(x, frequency), frequency = frequency))
}


#' Test if an object is a \code{\link{period_range}}
#'
#' @param x any R object
#' @return \code{TRUE} if the object is a \code{period_range}
#' @examples
#' range <- period_range("2016Q1/2017Q1")
#' is.period_range(range)
#' is.period_range("2016Q1/2017Q1")
#' @export
is.period_range <- function(x) {
  return(inherits(x, "period_range"))
}

# internal function to create a period_range from start, end
# and frequency
create_period_range <- function(start, end, frequency) {
  return(structure(c(start, end, frequency), class = "period_range"))
}

# binary operators (arithmetic and logical)
#' @export
Ops.period_range <- function(e1, e2) {
  if (.Generic %in% c("==", "!=", "<", ">", "<=", ">=")) {
    # logical operator
    if (is.period_range(e1) && is.period_range(e2)) {
      if (.Generic == "==") {
        return(identical(as.numeric(e1), as.numeric(e2)))
      } else if (.Generic == "!=") {
        return(!identical(as.numeric(e1), as.numeric(e2)))
      } else if (e1[3] != e2[3]) {
        stop(paste("Logical operations '<, <=, >, >=' on period_ranges",
                   "with different frequencies are not allowed"))
      } else {
        if (is.na(e1[1]) && is.na(e2[1])) {
          return(do.call(.Generic, list(e1[2], e2[2])))
        } else if (is.na(e1[2] && is.na(e2[2]))) {
          return(do.call(.Generic, list(e1[1], e2[1])))
        } else if (any(is.na(e1)) || any(is.na(e2))) {
          return(NA)
        } else {
          return(do.call(.Generic, list(e1[1], e2[1])) &&
                   do.call(.Generic, list(e1[2], e2[2])))
        }
      }
    } else {
      stop(paste("Both operators must be period_ranges",
                 "when using logical operators"))
    }
  } else if (.Generic %in% c("+", "-")) {
    # arithmetic operator + or -
    if (is.period_range(e1) && is.numeric(e2) && length(e2) == 1) {
      if (e2 != as.integer(e2)) {
        stop("Second operand must be an integer number")
      }
      # e1[ ] +/- e2
      start_end <- do.call(.Generic, list(e1[1:2], e2))
      retval <- create_period_range(start_end[1], start_end[2], e1[3])
    } else if (is.numeric(e1) && length(e1) == 1 && is.period_range(e2)) {
      if (e1 != as.integer(e1)) {
        stop("First operand must be an integer number")
      }
      # e2[ ] +/- e1
      start_end <- do.call(.Generic, list(e2[1:2], e1))
      retval <- create_period_range(start_end[1], start_end[2], e2[3])
    } else {
      stop(paste("Arithmetic operators + and - only allowed on a",
                 "combination of period_range and integer number"))
    }
    return(retval)
  } else {
    stop("Illegal operation, only + and - or logical operators allowed")
  }
}


#' @export
as.character.period_range <- function(x, ...) {
  if (!is.na(x[1])) {
    retval <- as.character.period(start_period(x))
    if (!is.na(x[2]) && x[2] == x[1]) {
      return(retval)
    }
  } else {
    retval <- ""
  }
  retval <- paste0(retval, "/")
  if (!is.na(x[2])) {
    retval <- paste0(retval, as.character.period(end_period(x)))
  }
  return (retval)
}



#' Return the number of periods in a \code{\link{period_range}} object.
#'
#' @param  x a \code{period_range}
#' @return The number of periods in the range, or \code{Inf} if the
#' range is not bounded
#' @examples
#' range <- period_range("2010Q2", "2011Q3")
#' nperiod(range)  # the result will be 6
#' @export
nperiod  <- function(x) {
  if (!inherits(x, "period_range")) {
    stop("Variable should be a period_range object")
  }
  if (is.na(x[1]) | is.na(x[2])) {
    return (Inf)
  }
  return(nperiod__(x))
}

nperiod__  <- function(x) {
  # Internal nperiod function, no check for NA's or variabletype
  return (x[2] - x[1] + 1)
}

#' @export
print.period_range <- function(x, ...) {
  print(as.character(x))
}
