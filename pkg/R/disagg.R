#' Temporal disaggregation of time series using spline interpolation.
#'
#' This function converts an timeseries to a timeseries with higher frequency,
#' for example a yearly timeseries to a quartely timeseries. Missing values
#' are obtained by spline interpolation.
#'
#' This function employs function \code{\link[stats]{spline}} of the
#' \code{stats} package
#'
#' @param x  a \code{\link[stats]{ts}} of \code{\link{regts}} object
#' @param nfrequency the frequency of the result. This should be higher than
#' the frequency of timeseries \code{x}.
#' @param method specifies the type of spline to be used.
#' Possible values are \code{"fmm"}, \code{"natural"}, \code{"periodic"},
#' \code{"monoH.FC"} and \code{"hyman"}. Can be abbreviated.
#' @param constraint Constraint on the high frequency result.
#' Possible values are \code{"sum"}, \code{"average"}, \code{"last"},
#' \code{"first"}.
#' Either the sum, the average, the first or the last value of the resulting
#' high-frequency series should be equal to the corresponding
#' low frequency value
#' @importFrom stats spline
#' @export
disagg <- function(x, nfrequency,
                   method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"),
                   constraint = c("sum", "average", "last", "first")) {

  constraint <- match.arg(constraint)

  # make sure that x is a matrix
  input_is_matrix <- is.matrix(x)

  if (!input_is_matrix) {
    dim(x) <- c(length(x), 1)
  }

  frac <- nfrequency / frequency(x)

  do_cumul <- constraint %in% c("sum", "average")

  if (do_cumul) {
    x[start_period(x) - 1] <- 0
    x[] <- apply(x, MARGIN = 2, FUN = cumsum)
  }

  p_start_inp <- start_period(x)

  # spline interpolation
  f_spline <- function(x, n) {
    return(spline(x = seq_len(n), y = x, n = n * frac - 2)$y)
  }
  x_splined <- apply(x, MARGIN = 2, FUN = f_spline, n = nrow(x))

  year <- get_year(p_start_inp)
  subp_inp <- get_subperiod(p_start_inp)
  if (constraint == "first") {
    subp_out <- (subp_inp - 1) * frac + 1
  } else {
    subp_out <- subp_inp * frac
  }

  p_start_out <- period(year, frequency = nfrequency) + (subp_out - 1)
  result <- regts(x_splined, start = p_start_out)

  if (!input_is_matrix) {
    dim(result) <- NULL
  }

  if (do_cumul) {
    result <- diff(result)
    if (constraint == "average") {
      result <- result * frac
    }
    return(result)
  } else {
    return(result)
  }
}
