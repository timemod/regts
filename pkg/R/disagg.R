#' Temporal disaggregation of time series using spline interpolation.
#'
#' This function converts an timeseries to a timeseries with higher frequency,
#' for example a yearly timeseries to a quarterly timeseries. Missing values
#' are obtained by spline interpolation. This function employs function
#' \code{\link[stats:splinefun]{spline}} of the \code{stats} package.
#'
#' For each individual timeseries, trailing and leading \code{NA} values are
#' removed before interpolation. If the timeseries contains intermediate
#' \code{NA} values,  the resulting timeseries is set to NA.
#'
#' For details about the different spline methods,
#' consult the documentation of the \code{\link[stats:splinefun]{spline}}
#' function of the \code{stats} package.
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
#' low-frequency value.
#' @importFrom stats spline
#' @export
disagg <- function(x, nfrequency,
                   method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"),
                   constraint = c("sum", "average", "last", "first")) {

  method <- match.arg(method)
  constraint <- match.arg(constraint)
  if (!is.numeric(x)) {
    stop("The input timeseries is not a numeric timeseries")
  }

  old_frequency <- frequency(x)
  if (nfrequency <= old_frequency) {
    stop(sprintf("nfrequency (%d) is not larger than the input frequency (%d)",
                 nfrequency, old_frequency))
  } else if (nfrequency %% old_frequency) {
    stop(sprintf(paste("nfrequency (%d) is not an integer multiple of the input",
                       "frequency (%d)"), nfrequency, old_frequency))
  }
  frac <- nfrequency / old_frequency


  do_cumul <- constraint %in% c("sum", "average")

  input_is_matrix <- is.matrix(x)
  if (input_is_matrix) {
    col_names <- colnames(x)
  }

  x <- na_trim(x)
  if (is.null(x)) {
    stop("Input timeseries contains only NA values")
  }

  # internal function to disaggregate a single timeseries
  disagg_uni <- function(x) {

    x_trim <- na_trim(x)

    if (is.null(x_trim)) {
      x_trim <- x
    }

    if (do_cumul) {
      x_trim[start_period(x_trim) - 1] <- 0
      x_trim[] <- cumsum(x_trim)
    }

    p_start_inp <- start_period(x_trim)
    year <- get_year(p_start_inp)
    subp_inp <- get_subperiod(p_start_inp)
    if (constraint == "first") {
      subp_out <- (subp_inp - 1) * frac + 1
    } else {
      subp_out <- subp_inp * frac
    }
    p_start_out <- period(year, frequency = nfrequency) + (subp_out - 1)
    n <- length(x_trim)
    nres <- (n - 1) * frac + 1
    if (any(is.na(x_trim))) {
      result <- rep(NA_real_, nres)
    } else {
      result  <- spline(x = seq_len(n), y = x_trim, n = nres,
                        method = method)$y
    }
    result <- regts(result, start = p_start_out)
    if (do_cumul) {
      result <- diff(result)
      if (constraint == "average") {
        result <- result * frac
      }
    }
    return(result)
  }

  l_input <- as.list(x)
  l_splined <- lapply(l_input, FUN = disagg_uni)
  result <- do.call(cbind, l_splined)

  if (input_is_matrix && !is.matrix(result)) {
    dim(result) <- c(length(result), 1)
    colnames(result) <- col_names
  }

  return(result)
}




