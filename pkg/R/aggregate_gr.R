#' Convert timeseries with absolute or relative changes to a lower
#' frequency
#'
#' This function implements temporal aggregation for timeseries with absolute,
#' relative or percentage changes. As shown in vignette
#'  \href{../doc/aggregation.pdf}{\emph{"Temporal Aggregation of
#' (Growth) Timeseries"}},
#' the standard function \code{\link[stats]{aggregate}} does not yield correct
#' results for these type of timeseries.
#'
#' There are methods for different types of input timeseries.
#' The \code{dif1s} and \code{dif1} methods assume that the input timeseries
#' contain a first difference (for \code{dif1s} the input is also
#' scaled). The result is a first difference in the output frequency.
#' The \code{pct} and \code{rel} methods assume timeseries that contain
#' percentage or relative change of a timeseries with only positive values.
#' They calculate
#' the exact percentage or relative change for the output timeseries.
#' More details for the various methods are provided in vignette
#'  \href{../doc/aggregation.pdf}{\emph{"Temporal Aggregation of
#' (Growth) Timeseries"}}.
#'
#' As explained before, the \code{pct} and \code{rel} methods assume timeseries
#' that contain percentage or relative change of a timeseries with only positive
#' values. This imposes restrictions on the input timeseries
#' `x`. For the `pct` method, `(1 + x / 100) >= 0`  and for
#' `rel`, `(1 + x) >= 0`. Function `aggregate_gr` gives an error  if these
#' conditions are not satisfied.
#' @param x  a \code{\link[stats]{ts}} or \code{\link{regts}} object
#' @param nfrequency the frequency of the result. This should be higher than
#' the frequency of timeseries \code{x}
#' @param method Aggregation method: \code{"dif1s"}, \code{"dif1"}, \code{"pct"}
#' or \code{"rel"}. See Details.
#' @return a \code{regts} with frequency \code{nfrequency}
#' @examples
#' ts_q <- regts(abs(rnorm(10)), start = "2016Q1")
#' aggregate_gr(ts_q, method = "dif1s")
#'
#' ts_m <- regts(matrix(abs(rnorm(20)), ncol = 2), start = "2017M1", names = c("a", "b"))
#' aggregate_gr(ts_m, method = "rel", nfrequency = 4)
#' @export
#' @useDynLib regts, .registration = TRUE
#' @importFrom Rcpp sourceCpp
aggregate_gr <- function(x, method = c("dif1s", "dif1", "pct", "rel"),
                         nfrequency = 1) {

  method <- match.arg(method)
  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be a numeric timeseries")
  }
  if (!is.matrix(x)) {
    is_mat <- FALSE
    dim(x) <- c(length(x), 1)
  } else {
    is_mat <- TRUE
  }

  if (method %in% c("pct", "rel")) check_growth_factors(x, is_mat, method)

  # call C++ function agg_gr (see src/agg_gr.cpp)
  res <- agg_gr(x, nfrequency, method)
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


check_growth_factors <- function(x, is_mat, method) {
  # Check for negative growth factors (1 + x). The pct and rel aggregation
  # methods assume that the timeseries are always positive. This is only
  # poossible if the growth factors are larger than or equal to zero.

  if (method == "pct") x <- x / 100

  problem_cols <- apply(x, FUN = function(x) {any(!is.na(x) & x < -1)},
                        MARGIN = 2)

  if (any(problem_cols)) {
    if (is_mat) {
      cnames <- colnames(x)
      if (is.null(cnames)) cnames <- seq_len(ncol(x))
      problem_col_names <- cnames[problem_cols]
      stop(paste0("Input timeseries contains negative growth factors",
                  " for columns: ", paste(problem_col_names, collapse = ", "),
                  "."))
    } else {
      stop("Input timeseries contains negative growth factors.")
    }
  }
  return(invisible(NULL))
}
