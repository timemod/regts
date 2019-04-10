#' Create a \code{\link{period}} object
#'
#' Function \code{period} creates a \code{period} object
#' based on a character string or numeric scalar.
#' Possible character string formats are for example \code{"2017Q2"},
#' \code{"2017m2"}, \code{"2017"} or \code{"2017-2"}.
#' Possible numeric formats are for example \code{2017} or
#' \code{2017.25} (the second quarter or the fourth month of 2017).
#' The function also accepts a \code{\link{Date}},
#' \code{\link[base:DateTimeClasses]{POSIXct}} or
#' \code{\link[base:DateTimeClasses]{POSIXlt}} argument.
#' See Details.
#' \cr\cr
#' Function \code{as.period}
#' coerces an R object to a \code{period} object if possible.
#'
#' The function \code{period} accepts a character string or
#' a numeric scalar as arguments. The specific format is described below.
#'
#' \strong{string format}
#'
#' The format for yearly periods is for example \code{"2017"}
#' or \code{"2017Y"} (the suffix \code{"Y"} is optional).
#'
#' The standard format format for quarterly periods is
#' for example \code{"2017Q3"}. Alternative formats
#' such as \code{"2017 3Q"} and \code{"2017.3Q"}
#' are also recognized. The separator between the year and the
#' quarter can be a blank or a dot, as in the previous examples,
#' but also a forward slash (\code{"/"}) and underscore (\code{"_"})
#' are allowed.
#'
#' The format for monthly periods is similar as that of
#' quarterly periods, except that the \code{"Q"} is replaced by
#' \code{"M"}.
#'
#' Periods with other frequencies than year, quarter and month can be specified
#' as for example \code{"2017-2"}. Alternative
#' separators (blank, dot, etc.) are possible. In this case argument
#' \code{frequency} should be specified.
#'
#' The string format is case insensitive, and may be prefixed
#' with \code{"Y"} or \code{"T"}. Thus for exampe \code{"t2017q3"}
#' is also an allowed period string.
#'
#' \strong{numeric format}
#'
#' An integer number, such as \code{2017} specifies a year,
#' or the first subperiod in a year if argument \code{frequency} has been
#' specified.
#'
#' If the numeric has a non-zero fractional part,
#' then argument \code{frequency} is mandatory,
#' For example, the numeric \code{2017.25}
#' can specify the second quarter of 2017 or the fourth month of 2017.
#'
#' \strong{\code{Date}, \code{POSIXct} and \code{POSIXlt}}
#'
#' The function also accepts a \code{\link{Date}},
#' \code{\link[base:DateTimeClasses]{POSIXct}} or
#' \code{\link[base:DateTimeClasses]{POSIXlt}} argument.
#' By default the function converts this object to a \code{period} with
#' frequency month. It is possible to specify another output frequency,
#' provided that this frequency is a divisor of 12.
#'
#' @param x a character, numeric scalar,
#' \code{\link[base]{Date}}, \code{\link[base:DateTimeClasses]{POSIXct}}
#' or \code{\link[base:Dates]{POSIXlt}}. If \code{x} has length > 1, then the
#' function returns of list of \code{period} objects.
#' @param frequency frequency of the period. Argument \code{frequency} is
#' mandatory if the frequency cannot be inferred from \code{x} (for example
#' \code{"2017-2"} could be a quarter, month, etc.)
#' @param ... additional arguments to be passed to or from methods (currently
#' not used in package \code{regts})
#' @return a \code{period} object if \code{length(x) == 1}, or a
#' list of \code{period} objects if  \code{length(x) > 1}
#'
#' @examples
#' period("2010Q3")
#' period("2010-2", frequency = 3)
#' period(2015)
#' period(2010.25, frequency = 4)
#'
#' # examples for as.period
#' as.period("2010q3")
#' p <- period("2010m11")
#' as.period(p)
#'
#' # example with a Date object
#' d <- Sys.Date()
#' period(d)
#'
#' # create a sequence of period objects
#' seq(period("2018q2"), period("2019q4"))
#' @importFrom Rcpp sourceCpp
#' @seealso \code{\link{period_range}} and \code{\link{seq}}
#' @export
period <- function(x, frequency = NA) {
  return(as.period(x, frequency = frequency))
}

#' @rdname period
#' @export
as.period <- function(x, ...) UseMethod("as.period")

#' @export
as.period.period <- function(x, ...) {
  return(x)
}

#' @export
as.period.character <- function(x, frequency = NA, ...) {
  # Call C++ function parse_period
  return(parse_period(x, frequency))
}

#' @export
as.period.factor <- function(x, frequency = NA, ...) {
  return(as.period.character(as.character(x), frequency = frequency, ...))
}

#' @export
as.period.numeric <- function(x, frequency = NA, ...) {

  # convert possible integer vector to numeric vector
  x <- as.numeric(x)

  if (is.na(frequency)) {
    if (any(is.na(x) | x %%1 != 0)) {
      stop("Argument frequency should be specified.")
    } else {
      frequency <- 1
    }
  } else if (frequency == 1 && !all(is.na(x) | x %% 1 == 0)) {
    stop("If frequency == 1, then x should be an integer.")
  }

  if (frequency == 1) {
    subp_count <- x
  } else {
    year <- floor(x)
    subp <- floor(frequency * (x %% 1))
    subp_count <- year * frequency + subp
  }

  if (length(subp_count) == 1) {
    return(create_period(subp_count, frequency))
  } else {
    return(create_periods(subp_count, frequency))
  }
}

#' @export
as.period.logical <- function(x, frequency = NA, ...) {
  return(as.period.numeric(as.numeric(x), frequency = frequency, ...))
}

#' @export
as.period.Date <- function(x, frequency = NA, ...) {
  return(as.period.POSIXlt(as.POSIXlt(x), frequency))
}

#' @export
as.period.POSIXct <- function(x, frequency = NA, ...) {
  return(as.period.POSIXlt(as.POSIXlt(x), frequency))
}

#' @export
as.period.POSIXlt <- function(x, frequency = NA, ...) {

  #if (length(x) != 1) {
  #  return(lapply(x, FUN = as.period.POSIXlt, frequency = frequency, ...))
  #}

  if (is.na(frequency)) {
    frequency <- 12
  }

  year <- x$year + 1900
  month <- x$mon + 1

  if (frequency != 12) {
    if (12 %% frequency != 0) {
      stop(sprintf("12 is not divisibly by te specified frequency (%d)"))
    } else {
      subperiod <- floor((month - 1) * frequency / 12 + 1)
    }
  } else {
    subperiod <- month
  }
  subp_since_christ <- year * frequency + subperiod - 1
  if (length(subp_since_christ) == 1) {
    return(create_period(subp_since_christ, frequency))
  } else {
    return(create_periods(subp_since_christ, frequency))
  }
}


#' Test if an object is a \code{\link{period}}
#'
#' @param x any R object
#' @return \code{TRUE} if the object is a \code{period}
#' @examples
#' p <- period("2016Q1")
#' is.period(p)
#' is.period("2016Q1")
#' @export
is.period <- function(x) {
  return(inherits(x, "period"))
}

# binary operators (arithmetic and logical)
#' @export
Ops.period <- function(e1, e2) {

  if (.Generic %in% c("==", "!=", "<", ">", "<=", ">=")) {
    # logical operator
    if (is.period(e1) && is.period(e2) &&
        (attr(e1, 'frequency') != attr(e2, 'frequency'))) {
      # if e1 and e2 are both periods with a different frequency,
      # the operators == and != are meaningful, but comparison
      # operators such as > do not make sense.
      if (.Generic %in% c("==", "!=")) {
        return(.Generic != "==")
      } else {
        stop(sprintf(paste("Illegal logical operator %s, only == and !=",
                           "allowed if the two periods have different",
                           "frequencies."), .Generic))
      }
    }
    return(NextMethod(.Generic))

  } else if (.Generic %in% c("+", "-")) {
    # arithmetic operator + or -

    e1_is_per <- is.period(e1)
    e2_is_per <- is.period(e2)

    if (e1_is_per && e2_is_per) {
      if (attr(e1, 'frequency') != attr(e2, 'frequency')) {
        stop(paste("Arithmetic operations on periods with different",
                   "frequencies are not allowed."))
      }
      if (.Generic == "+") {
        stop("Arithmetic operation + on two periods is not allowed.")
      }
    } else {
      # check if supplied numeric arguments are integers
      if (!e1_is_per && is.numeric(e1) &&  !all(is.na(e1) | e1 %% 1 == 0)) {
        stop("First operand contains non-integer values.")
      } else if (!e2_is_per && is.numeric(e2) && !all(is.na(e2) | e2 %% 1 == 0)) {
        stop("Second operand contains non-integer values.")
      }
    }

    # perform the actual arithmetic operation
    retval <- NextMethod(.Generic)

    if (e1_is_per && e2_is_per && .Generic == "-") {
      # if both operands are periods, then the result is an ordinary numeric.
      retval <- as.numeric(retval)
    } else if (length(retval) > 1) {
      # the result is a list of periods
      frequency <- if (e1_is_per) frequency(e1) else frequency(e2)
      retval <- create_periods(as.numeric(retval), frequency = frequency)
    }
    return(retval)

  } else {

    stop(sprintf("Operator %s not implemented for period objects.", .Generic))
  }
}

#' @export
as.character.period <- function(x, ...) {
  freq <- frequency(x)
  if (freq == 1) {
    return(as.character(as.numeric(x)))
  } else {
    if (freq == 4) {
      freq_char <- "Q"
    } else if (freq == 12) {
      freq_char <- "M"
    } else {
      freq_char <- "-"
    }
    format <- sprintf("%%d%%s%%0%dd", ceiling(log10(freq)))
    return(sprintf(format, get_year__(x), freq_char, get_subperiod__(x)))
  }
}


# internal function
get_year__ <- function(x) {
  return(as.numeric(x) %/% frequency(x))
}

# internal function
get_subperiod__ <- function(x) {
  return(as.numeric(x) %% frequency(x) + 1)
}

#' Return the year of a \code{\link{period}}
#' @param x a \code{\link{period}}
#' @return the year
#' @examples
#' get_year(period("2010Q3"))
#' @seealso \code{\link{get_subperiod}}
#' @export
get_year <- function(x) {
  if (!is.period(x)) {
    stop("Argument x is not a period")
  }
  return(get_year__(x))
}

#' @export
as.Date.period <- function(x, ...) {

  # first change frequency to month
  freq_x  <- frequency(x)

  if (12 %% freq_x != 0) {
    stop(sprintf(paste("12 is not divisible by frequency timeseries",
                       "(%d)."), freq_x))
  }
  per_m <- create_period(floor(12 * x[1] / freq_x), 12)

  year <- get_year(per_m)
  month <- get_subperiod(per_m)
  date_text <- paste0(year, "-", month, "-01")
  return(as.Date(date_text))
}

#' Return the subperiod of a \code{\link{period}}
#'
#' This function returns the subperiod within a year.
#' For example, for \code{period} \code{2011Q3} the function
#' returns 3.
#' @param x a \code{period}
#' @return the subperiod of a \code{period}
#' @examples
#' get_subperiod(period("2010Q3"))
#' @seealso \code{\link{get_year}}
#' @export
get_subperiod <- function(x) {
  if (!is.period(x)) {
    stop("Argument x is not a period")
  }
  return(get_subperiod__(x))
}

#' @export
print.period <- function(x, ...) {
  if (frequency(x) == 1) {
    print(as.numeric(x))
  } else {
    print(as.character(x))
  }
}


# Create a period object based on the number of subperiods after Christ.
# Internal function.
create_period <- function(subperiod_count, frequency) {
  return(structure(subperiod_count[1], class = "period",
                    frequency = frequency))
}

# Internal function that creates a list of period objects based
# on a vector with the number of subperiods after Christ.
create_periods <- function(subperiod_count, frequency) {

  fun <- function(x) {
    attr(x, "class") <- "period"
    attr(x, "frequency") <- frequency
    return(x)
  }
  return(lapply(subperiod_count, FUN = fun))
}

# minimum or maximum of 2 or more periods
#' @export
Summary.period <- function(..., na.rm = FALSE){

  if (.Generic %in% c("min", "max")) {

    args <- list(...)

    # check if period
    is_prd <- sapply(args, FUN = is.period)
    if (!all(is_prd))  {
      stop("Inputs must all be periods")
    }

    # check frequencies
    freq <- sapply(args, FUN = frequency)
    if (length(unique(freq)) > 1){
      stop("All periods must have the same frequency")
    }

    result <- NextMethod(.Generic)
    return(create_period(result, freq[[1]]))
  }
  else {
    stop(paste(.Generic, "is not supported for period objects"))
  }
}


#' Generates a sequence of periods
#'
#' Generates a regular sequence of \code{\link[regts]{period}} objects. The
#' function returns the sequence as a \code{\link{list}}.
#' @param from a \code{period} object specifying the first period of the
#'   sequence.
#' @param to a \code{period} object (or an object that can be coerced to a
#'   \code{period} object) specifying the last period of the sequence
#' @param by an integer number, the increment of the sequence (the number of
#' periods between each period in the sequence).
#' @param length.out the desired length of the sequence. A non-negative number.
#'   If both \code{from} and \code{to} have been specified, and if
#'   \code{length.out} > 1, then the number of periods between \code{from} and
#'   \code{to} should be divisible by \code{length.out - 1}.
#' @param ... arguments passed to or from methods (not used)
#' @return a \code{\link{list}} with period objects
#' @name seq
#' @examples
#' p1 <- period("2018q2")
#' seq(p1, length.out = 4)
#' seq(p1, "2019q4")
#' seq(p1, "2019q4", by = 2)
#' @seealso \code{\link[regts]{period}} and \code{\link[regts]{period_range}}
#' @export
seq.period <- function(from, to, by, length.out, ...) {

  from_missing <- missing(from)
  to_missing <- missing(to)

  if (from_missing && to_missing) {
    stop("For seq.period, either from or to must be specified")
  }

  if (!from_missing) {
    f_from <- frequency(from)
    per_from <- from
    from <- as.numeric(from)
  }
  if (!to_missing) {
    to <- as.period(to)
    f_to <- frequency(to)
    per_to <- to
    to <- as.numeric(to)
  }

  freq <- if (to_missing) f_from else f_to

  if (!from_missing && !to_missing && f_from != f_to) {
    stop(sprintf(paste("Argument to has a different frequency (%d) than",
                       "argument from (%d)"), f_to, f_from))
  }

  if (!missing(by)) {
     if (by %% 1 != 0) {
       stop("Argument by is not an integer")
     }
  } else if (!missing(length.out) && length.out > 1 && !from_missing
             && !to_missing) {
    if ((to - from) %% (length.out - 1) != 0) {
      stop(sprintf(paste("The number of periods (%d) between %s and %s",
                  "is not divisible by\nlength.out - 1 = %d."),
                 to - from, as.character(per_from), as.character(per_to),
                 length.out - 1))
    }
    by <- (to - from) / (length.out - 1)
  }


  # subperiod_count is the number of subperiods since Christ
  subperiod_count <- NextMethod(.Generic)

  return(create_periods(as.numeric(subperiod_count), frequency = freq))
}

#' @export
c.period <- function(...) {
  return(list(...))
}

