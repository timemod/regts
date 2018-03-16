#' Disaggregation of time series using cubic spline interpolation.
#'
#' This function converts a timeseries to a timeseries with higher frequency,
#' for example a yearly timeseries to a quarterly timeseries.
#' Cubic spline interpolation is used to interpolate between the low frequency
#' observations.
#'
#' Argument \code{conds} can be used to select the boundary conditions
#' for the cubic spline interpolation.
#' Choose \code{"natural"} for a natural cubic spline
#' (zero second derivatives at the end points).
#' For \code{"not-a-knot"} the third derivative is continuous at the
#' second and one but last point.
#'
#' Leading and trailing \code{NA} values are removed before the interpolation.
#'
#' @param x  a  \code{\link{regts}}  or \code{\link[stats]{ts}} object
#' @param nfrequency the frequency of the result. This should be higher than
#' the frequency of timeseries \code{x}.
#' @param conds a character specifying the boundary conditions:
#' \code{"natural"} or \code{"not-a-knot"}. Default is \code{"natural"}.
#' See details.
#' @param constraint Constraint on the high frequency result.
#' Possible values are \code{"average"}, \code{"sum"}, \code{"first"} and
#' \code{"last"}.
#' Either the average, the sum, the first or last value of the resulting
#' high-frequency series should be equal to the corresponding
#' low-frequency value.
#' @examples
#' # construct quarterly series
#' q <- regts(matrix(c(1:3, NA, -3, -5),  ncol = 2),
#'            period = "2017Q2/2017Q4", names = c("a", "b"))
#'
#' disagg(q, nfrequency = 12)
#'
#' disagg(q, constraint = "last", conds = "not-a-knot", nfrequency = 12)
#' @seealso Alternative spline methods are available in package
#' \code{\link[tempdisagg:tempdisagg-package]{tempdisagg}}
#' @export
disagg <- function(x, nfrequency,
                   constraint = c("average", "sum", "first", "last"),
                   conds = c("natural", "not-a-knot")) {

  conds <- match.arg(conds)
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

  # call C++ function disagg_sline (see src/disagg_spline.cpp)
  res <- disagg_spline(x, nfrequency, constraint, conds)
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
