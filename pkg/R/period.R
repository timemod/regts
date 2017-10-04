#' Create a \code{\link{period}} object based on a character string or numeric.
#'
#' Possible character string formats are for example \code{"2010Q2"},
#' \code{"2010M2"}, \code{"2011"} or \code{"2011-1"}.
#' The period may be prefixed with a \code{"T"} or \code{"Y"}.
#' Possible numeric formats are \code{2011} or \code{2011.25}
#' @param x a character string or numeric
#' @param frequency frequency of the period object.
#' Argument \code{frequency} is mandatory if a general period format
#' such as \code{"2011-1"} has been specified
#' @return a \code{period} object
#'
#' @name period
#' @examples
#' period("2010Q3")
#' period("2010-2", frequency = 3)
#' period(2015)
#' period(2010.25, frequency = 4)
#'
#' as.period("2010Q3")
#' as.period(2010)
#' @importFrom Rcpp sourceCpp
NULL

#' @rdname period
#' @export
period <- function(x, frequency = NA) {
  return (as.period(x, frequency = frequency))
}

#' @rdname period
#' @export
as.period <- function(x, ...) UseMethod("as.period")

#' @export
as.period.period <- function(x, ...) {
  return (x)
}

#' @export
as.period.character <- function(x, frequency = NA, ...) {
  return (parse_period(x, frequency = frequency))
}

#' @export
as.period.numeric <- function(x, frequency = NA, ...) {
  if (all.equal(x, as.integer(x)) == TRUE) {
    if (is.na(frequency) || frequency == 1) {
      return (create_period(as.numeric(x) , 1))
    }
  } else if (is.na(frequency)) {
    stop("Argument frequency should be specified")
  }
  year <- floor(x)
  print(year)
  subp <- floor(frequency * (x %% 1))
  print(subp)
  return (create_period(year * frequency + subp , frequency))
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
  return (inherits(x, "period"))
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
    return (NextMethod(.Generic))
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

    }
    # the return value should always be an integer
    return (floor(retval))
  } else {
    stop("Illegal arithmetic operation, only + and - allowed")
  }
}

#' @export
as.character.period <- function(x, ...) {
  freq <- frequency(x)
  if (freq == 1) {
    return (as.character(as.numeric(x)))
  } else {
    if (freq == 4) {
      freq_char <- "Q"
    } else if (freq == 12) {
      freq_char <- "M"
    } else {
      freq_char <- "-"
    }
    return (paste0(get_year__(x), freq_char, get_subperiod__(x)))
  }
}


# internal function
get_year__ <- function(x) {
  return (as.numeric(x) %/% frequency(x))
}

# internal function
get_subperiod__ <- function(x) {
  return (as.numeric(x) %% frequency(x) + 1)
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
  return (get_year__(x))
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
  return (get_subperiod__(x))
}

#' @export
print.period <- function(x, ...) {
  print(as.character(x))
}



# Create a period object based on the number of subperiods after Christ.
# Internal function.
create_period <- function(subperiod_count, frequency) {
  return (structure(subperiod_count, class = "period",
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



