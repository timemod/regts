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
#' has no lower or upper bound.
#'
#' @param p1 the first period (a \code{period}, an object that can be coerced
#' to a \code{period}, or \code{NULL}). If \code{p1} is \code{NULL} the lower
#' bound of the period range is undetermined. \code{p1} can also be
#' a character string specifying a period range, for example
#' \code{"2010Q2/2011Q3"}).

#' @param p2 the last period (a \code{period}, an object that can be coerced
#' to a \code{period}, or \code{NULL}).
#' If \code{p2} is \code{NULL}, the upper bound of the period range is undetermined.
#' @param x an R object
#' @param frequency frequency of the period objects. This argument is mandatory
#' if argument \code{p1} or \code{p2} is a character with general period format
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
#' p1 <- period("2013Q3")
#' period_range(p1, p1 + 5)
#'
#' # create a period_range from 2010Q2 with no upper bound
#' period_range("2010Q2", NULL)
#'
#' # create a period_range for a timeseries with frequency 2 (half year)
#' period_range("2010-2", "2016-2", frequency = 2)
#'
#' # convert a period object to a period_range with equal start and end period
#' p <- period("2010Q2")
#' as.period_range(p)
#' @seealso \code{\link{period}}, \code{\link{nperiod}},
#' \code{\link{start_period}} and \code{\link{end_period}}
#' @export
period_range <- function(p1, p2 = p1, frequency = NA) {
  if (is.null(p1) && is.null(p2)) {
    stop("At least one of the periods should not be NULL")
  }
  if (missing(p2)) {
    # direct conversion, inputs "2016q1" and "2016q1/2017q1" are possible
    return(as.period_range(p1, frequency))
  } else if (!is.null(p1)) {
    if (is.character(p1) && grepl("/", p1[1])) {
      stop("Argument p2 should not be specified if p1 is a period range string")
    }
    p1 <- as.period(p1, frequency)
    freq1 <- attr(p1, 'frequency')
  }
  if (!is.null(p2)) {
    p2 <- as.period(p2, frequency)
    freq2 <- attr(p2, 'frequency')
  }
  if ((!(is.null(p1) || is.null(p2)))) {
    if (freq1 != freq2) {
      stop("The two periods have different frequency")
    }
    if (p2 < p1) {
      stop(paste0("The start period (", p1, ") is after the end period (", p2, ")"))
    }
  }
  if (!is.null(p1)) {
    freq <- freq1
  } else {
    freq <- freq2
  }

  # convert periods to normal numbers
  if (!is.null(p1)) {
    p1 <- as.numeric(p1)
  } else {
    p1 <- NA_real_
  }
  if (!is.null(p2)) {
    p2 <- as.numeric(p2)
  } else {
    p2 <- NA_real_
  }
  return(create_period_range(p1, p2, freq))
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
  p1 <- as.period(x, frequency, ...)
  return(as.period_range(p1))
}

#' @export
as.period_range.POSIXlt <- function(x, frequency = 12, ...) {
  if (is.na(frequency)) {
    # this situation could happen is as.period.Date is called from
    # function period_range
    frequency <- 12
  }
  return(period_range(as.period(x, frequency), frequency = frequency))
}

#' @export
as.period_range.POSIXct <- function(x, frequency = 12, ...) {
  if (is.na(frequency)) {
    # this situation could happen is as.period.Date is called from
    # function period_range
    frequency <- 12
  }
  return(period_range(as.period(x, frequency), frequency = frequency))
}

#' @export
as.period_range.Date <- function(x, frequency = 12, ...) {
  if (is.na(frequency)) {
    # this situation could happen is as.period.Date is called from
    # function period_range
    frequency <- 12
  }
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
