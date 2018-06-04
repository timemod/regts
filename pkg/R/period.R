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
#' @param x a character string, numeric scalar,
#' \code{\link[base]{Date}}, \code{\link[base:DateTimeClasses]{POSIXct}}
#' or  \code{\link[base:Dates]{POSIXlt}}
#' @param frequency frequency of the period. Argument \code{frequency} is
#' mandatory if the frequency cannot be inferred from \code{x} (for example
#' \code{"2017-2"} could be a quarter, month, etc.)
#' @param ... additional arguments to be passed to or from methods (currently
#' not used in package \code{regts})
#' @return a \code{period} object
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
#' @importFrom Rcpp sourceCpp
#' @seealso \code{\link{period_range}}
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
  if (length(x) != 1) {
    stop("x should be a single character string")
  }
  return(parse_period(x, frequency = frequency))
}

#' @export
as.period.numeric <- function(x, frequency = NA, ...) {
  if (length(x) != 1) {
    stop("x should be a numeric scalar")
  }
  if (all.equal(x, as.integer(x)) == TRUE) {
    if (is.na(frequency) || frequency == 1) {
      return (create_period(as.numeric(x) , 1))
    }
  } else if (is.na(frequency)) {
    stop("Argument frequency should be specified")
  }
  year <- floor(x)
  subp <- floor(frequency * (x %% 1))
  return(create_period(year * frequency + subp , frequency))
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
  return(create_period(subp_since_christ, frequency))
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
        return (.Generic != "==")
      } else {
        stop("Illegal logical operation, only == and != allowed")
      }
    }
    return(NextMethod(.Generic))
  } else if (.Generic %in% c("+", "-")) {
    # arithmetic operator + or -
    retval <- NextMethod(.Generic)
    if (is.period(e1) && is.period(e2)) {
      if (attr(e1, 'frequency') != attr(e2, 'frequency')) {
        stop(paste("Arithmetic operations on periods with different",
                   "frequencies are not allowed"))
      }
      if (.Generic == "-") {
        # if the second operand is also a period, then the result
        # is an ordinary number.
        retval <- as.numeric(retval)
      } else {
        stop("Arithmetic operation + on two periods is not allowed")
      }
    } else {
      # one of the operands is not a period object
      if (anyNA(retval)) {
        stop("NA values in arithmetic operations with period objects")
      }
      if (!all(retval == round(retval))) {
          stop(paste("Arithmetic operations with periods are only possible",
                     "with integer operands"))
      }
      if (length(retval) > 1) {
        frequency <- if (is.period(e1)) frequency(e1) else frequency(e2)
        retval <- lapply(retval, FUN = create_period, frequency = frequency)
      }
    }
    return(retval)
  } else {
    stop("Illegal arithmetic operation, only + and - allowed")
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
  print(as.character(x))
}



# Create a period object based on the number of subperiods after Christ.
# Internal function.
create_period <- function(subperiod_count, frequency) {
  return(structure(subperiod_count, class = "period",
                    frequency = frequency))
}


# PRIVATE METHODS THAT COULD BE USEFULL FOR AUTOMATIC CONVERSIONS
# OF DATA FRAMES to TS

# Checks if character or numerical values can be converted to a
# period_range with a specific frequency
# INPUT
# x a character or numeric vector
# freq frequency (or NA: the period text should have either the
# specific frequency or an unknown frequyency.
#
# RETURN
# a logical vector with element equal to \code{TRUE} is the
# corresponding element in \code{x} can be converted to a
# period_range
is_period_text <- function(x, frequency = NA) {
  if (is.numeric(x)) {
    x <- as.character(x)
  }
  return(is_period_text_(x, frequency))
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



