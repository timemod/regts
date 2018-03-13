#' Temporal disaggregation of time series using spline interpolation.
#'
#' This function converts an timeseries to a timeseries with higher frequency,
#' for example a yearly timeseries to a quarterly timeseries. Missing values
#' are obtained by spline interpolation.
#'
#' For each individual timeseries, trailing and leading \code{NA} values are
#' removed before interpolation. If the timeseries contains intermediate
#' \code{NA} values,  the resulting timeseries is set to \code{NA}.
#'
#' Argument \code{method} can be used to select a spline interpolation
#' methods. All methods except \code{"nakn"} employ
#' the function code{\link[stats:splinefun]{spline}}
#' function of the \code{stats} package. More information
#' about the varous methods can be found in the documentation of that
#' package.
#'
#' For method \code{"nakn"} the function {\link[cspline]{cspline}}
#' of package \code{cspline} is used. This method used a not-a-knot
#' boundary conditions, which implies that the first derivative
#' at the second and one but last point are continuous.
#'
#' @param x  a \code{\link[stats]{ts}} object
#' @param nfrequency the frequency of the result. This should be higher than
#' the frequency of timeseries \code{x}.
#' @param method specifies the type of spline to be used.
#' Possible values are \code{"fmm"}, \code{"natural"}, \code{"periodic"},
#' \code{"monoH.FC"}, \code{"hyman"} and \code{"nakn"}. Can be abbreviated.
#' @param constraint Constraint on the high frequency result.
#' Possible values are \code{"sum"}, \code{"average"}, \code{"last"},
#' \code{"first"}.
#' Either the sum, the average, the first or the last value of the resulting
#' high-frequency series should be equal to the corresponding
#' low-frequency value.
#' @importFrom stats spline
#' @importFrom cspline cspline
#' @export
disagg_new <- function(x, nfrequency,
                       constraint = c("sum", "average", "last", "first"),
                       method = c("nakn", "natural")) {

  method <- match.arg(method)
  constraint <- match.arg(constraint)

  if (!is.numeric(x)) {
    stop("The input timeseries is not a numeric timeseries")
  }

  if (!is.matrix(x)) {
    is_mat <- FALSE
    dim(x) <- c(length(x), 1)
  } else {
    is_mat <- TRUE
  }

  # call C++ function agg_gr (see src/agg_gr.cpp)
  res <- disagg_spline(x, nfrequency, constraint, method)
  data  <- res[[1]]
  range_new <- res[[2]]

  if (is_mat) {
    colnames(data) <- colnames(x)
  } else {
    dim(data) <- NULL
  }

  return(create_regts(data, range_new[1], range_new[2], range_new[3],
                       ts_labels(x)))
}




