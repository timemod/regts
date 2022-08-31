#' Change the frequency of a \code{\link{period}} or \code{\link{period_range}}
#' object.
#'
#' A \code{period} can be converted to lower frequency. For example,
#' a month \code{"2017M4"} can be converted to the quarter \code{"2017Q2"}.
#' The old frequency should be divisible by the new frequency.
#' \cr\cr
#' A \code{period_range} can be converted to both lower and higher frequency.
#' For example, the range \code{"2017Q3/2018Q1"} can be converted to
#' the month range \code{"2017M7/2018M3"} or the year range
#' \code{"2017/2018"}.
#' If the \code{period_range} is converted to lower frequency, then the
#' old frequency should be divisible by the new frequency.
#' If the range is converted to higher frequency, then
#' the new frequency should be divisible by the old frequency.
#'
#' @param x a \code{\link{period}} or \code{\link{period_range}}
#' @param new_frequency the new_frequency
#' @param ... arguments passed to methods (not used in package \code{regts})
#' @return a \code{period} or \code{period_range} (depending on the type
#' of argument \code{x}) with the new frequency
#'
#' @examples
#' p <- period("2017M4")
#' change_frequency(p, 4)
#'
#' range <- period_range("2017Q3/2018Q1")
#' change_frequency(range, 12)
#' change_frequency(range, 1)
#' @export
change_frequency <- function(x, new_frequency, ...) {
  UseMethod("change_frequency")
}

#' @describeIn change_frequency Change the frequency of a \code{period} to
#' lower frequency
#' @export
change_frequency.period <- function(x, new_frequency, ...) {

  check_frequency_arg(new_frequency, "new_frequency")

  old_frequency <- frequency(x)

  if (new_frequency == old_frequency) {

    return(x)

  } else if (new_frequency > old_frequency) {

    stop(paste0("It is not possible to convert a period to higher frequency.\n",
                "Consider converting it to a period_range."))

  } else {

    if (old_frequency %%  new_frequency != 0) {
      stop(sprintf(paste("Old frequency (%d) not divisible by new frequency",
                         "(%d)."), old_frequency, new_frequency))
    }

    fac <- new_frequency / old_frequency
    p <- floor(as.numeric(x) * fac)
    return(create_period(p, new_frequency))
  }
}

#' @describeIn change_frequency Change the frequency of a \code{period_range} to
#' higher or lower frequency
#' @export
change_frequency.period_range <- function(x, new_frequency, ...) {

  check_frequency_arg(new_frequency, "new_frequency")

  old_frequency <- frequency(x)

  if (new_frequency == old_frequency) {

    return(x)

  } else if (new_frequency > old_frequency) {

    if (new_frequency %%  old_frequency != 0) {
      stop(sprintf(paste("New frequency (%d) not divisible by old frequency",
                         "(%d)."), new_frequency, old_frequency))
    }

    fac <- new_frequency / old_frequency

    p1 <- floor(x[1] * fac)
    p2 <- floor((x[2] + 1) * fac - 1)

  } else {

    if (old_frequency %%  new_frequency != 0) {
      stop(sprintf(paste("Old frequency (%d) not divisible by new frequency",
                         "(%d)."), old_frequency, new_frequency))
    }

    fac <- new_frequency / old_frequency

    p1 <- floor(x[1] * fac)
    p2 <- floor(x[2] * fac)
  }

  return(create_period_range(p1, p2, new_frequency))
}
