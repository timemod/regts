#' Generates a sequence of periods
#'
#' Generates a regular sequence of \code{\link[regts]{period}} objects.
#'
#' If both `from` and `to` are specified, then the two periods must have
#' the same frequency.
#'
#' @param from a \code{period} object or a character specifying the first period
#' of the sequence. If `from` is a character, it is coerced to a period object
#' with function \code{\link{as.period}}.
#' @param to a \code{period} object or a character
#'  specifying the last period of the sequence.
#' @param by an integer number, the increment of the sequence (the number of
#' periods between each period in the sequence).
#' @param length.out the desired length of the sequence. A non-negative number.
#'   If both \code{from} and \code{to} have been specified, and if
#'   \code{length.out} > 1, then the number of periods between \code{from} and
#'   \code{to} should be divisible by \code{length.out - 1}.
#' @param ... arguments passed to or from methods (not used)
#' @return a \code{period} vector
#' @name seq
#' @examples
#' p1 <- period("2018q2")
#' seq(p1, length.out = 4)
#' seq(p1, "2019q4")
#' seq(p1, "2019q4", by = 2)
#' seq("2018q1", "2020q1", by = 4)
#'
#' # example: print the value of a timeseries for every first quarter of a year
#' x <- regts(1:10, start = "2018q1")
#' seqp <- seq("2018q1", "2020q1", by = 4)
#' for (prd in as.list(seqp)) {
#'   cat(sprintf("x[%s] = %g\n", prd, x[prd]))
#' }
#' # Note that we do not loop directly over the period vector, but first convert
#' # the vector to a list. Otherwise the period class is lost.
#'
#' @seealso \code{\link[regts]{period}}, \code{\link[regts]{period_range}} and
#' \code{\link[regts]{get_periods}}
#' @export
seq.period <- function(from, to, by, length.out, ...) {

  from_missing <- missing(from)
  to_missing <- missing(to)

  if (from_missing && to_missing) {
    stop("For seq.period, either 'from' or 'to' must be specified")
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
    stop(sprintf(paste("Argument 'to' has a different frequency (%d) than",
                       "argument 'from' (%d)"), f_to, f_from))
  }

  if (!missing(by)) {
    if (by %% 1 != 0) {
      stop("Argument 'by' is not an integer")
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
  return(create_period(as.numeric(subperiod_count), frequency = freq))
}

#' @export
#' @rdname seq
seq.character <- function(from, to, by, length.out = NULL, ...) {

  #
  # NOTE: seq.character contains a lot of the same code as in seq.period,
  # but all attempts to combine the code failed. The problem is the handling
  # of missing length.out.
  #

  from_missing <- missing(from)
  to_missing <- missing(to)

  if (from_missing && to_missing) {
    stop("For seq.period, either 'from' or 'to' must be specified")
  }

  if (!from_missing) {
    from <- as.period(from)
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
    stop(sprintf(paste("Argument 'to' has a different frequency (%d) than",
                       "argument 'from' (%d)"), f_to, f_from))
  }

  if (!missing(by)) {
    if (by %% 1 != 0) {
      stop("Argument 'by' is not an integer")
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
  return(create_period(as.numeric(subperiod_count), frequency = freq))

}
