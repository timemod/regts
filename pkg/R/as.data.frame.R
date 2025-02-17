#' Convert a \code{\link{regts}} to a \code{\link[base]{data.frame}}
#'
#' @details

#' Three different format for the data frame are possble.
#'
#' **1. columnwise (default)**
#'
#' This is the default format. There is a column for each variable. Example:
#'
#' ```
#'        a   b
#' 2022   1  10
#' 2023   2  20
#' ```
#'
#' By default, the row names contain the periods of the timeseries, but if
#' argument `row_names = FALSE`, the first column named `period` will contain
#' the variable names, as shown below:
#'
#' ```
#' period  a   b
#' 2022    1  10
#' 2023    2  20
#' ````
#'
#' If the \code{regts} has labels, then each column of the data frame gets an
#' attribute named `label` with the label as value. These labels are shown when
#' the data frame is opened in the Data Viewer of RStudio. The labels can
#' also be retrieved by using function \code{\link[labelled]{var_label}} from
#' package `labelled`.
#'
#' **2. rowwise**
#'
#' For a rowwise data frame, there is a column for each period. For example,
#' ```
#'       label       2022  2023
#' a     Variable a     1     2
#' b     Variable b    10    20
#' ```
#' Use argument `rowwise = TRUE` to create a rowwise data frame.

#' By default, the row names contain the variable names, but if
#' argument `row_names = FALSE`, the first column named `name` will contain
#' the variable names. For example:
#'```
#' name   label       2022  2023
#' a      Variable a     1     2
#' b      Variable b    10    20
#' ```

#' If the timeseries has no labels the `label`, column is missing.
#'
#' **3. long format**
#'
#' For a data frame with long format, there is one row for each observation.
#' For example:
#'
#' ```
#'  name  label        period value
#'  a     Variable a     2022     1
#'  a     Variable a     2023     2
#'  b     Variable b     2022    10
#'  b     Variable b     2023    20
#' ````
#' Use argument `long = TRUE` to create such a data frame.
#' Argument `row_names` is ignored when the long format option is used.
#'
#' If the timeseries has no labels, the `label` column is missing.
#'
#' @param x a \code{\link{regts}}
#' @param rowwise a logical value: should the timeseries be stored rowwise
#' or columnwise in the data frame? Defaults to \code{FALSE}.
#' Ignored if `long = TRUE`.
#' @param row_names Whether to create row names. If \code{FALSE},
#' then an additional column with name \code{"period"} or \code{"name"} is
#' created for columnwise or rowwise timeseries, respectively.
#' Ignored if `long = TRUE` (for the long format the result will have
#' no row names).
#' @param period_as_date A logical (default \code{FALSE}).
#' If \code{TRUE} the periods are stored as \code{\link[base]{Date}} objects.
#' Depending on arguments \code{rowwise} and \code{row_names}
#' the periods may appear in the row or column names of the result data frame.
#' In that case the dates are coerced to character vectors,
#' using the standard date format \code{"\%Y-\%m-\%d"}
#' (see the documentation of function \code{\link[base]{strptime}}
#' for more information about date formats).
#' @param long Return the result is so called long format, i.e. a
#' data frame with one row for each observation. The result is a data frame
#' with columns `name`, `period` and `value` (if the timeseries has labels,
#' there will be an additional column `label` after the column `name`).
#' See Details.
#' @param ... additional arguments to be passed to methods.
#' @return A \code{\link[base]{data.frame}}
#' @name as.data.frame
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect where
#' @export
#' @examples
#' ts <- regts(matrix(1:4, ncol = 2) , start = "2015Q3", names = c("a", "b"),
#'            labels = c("Timeseries a", "Timeseries b"))
#' print(as.data.frame(ts))
as.data.frame.regts <- function(x, ..., rowwise = FALSE, row_names = TRUE,
                                period_as_date = FALSE, long = FALSE) {

  if (long) {
    if (!missing(row_names)) {
      warning("Argument 'row_names' is ignored if long is TRUE")
    }
    if (!missing(rowwise)) {
      warning("Argument 'rowwise' is ignored if long is TRUE")
    }
    # For the conversion a timeseries to long format, the handling of labels
    # is easier when the data is first converted to rowwise data frame.
    rowwise <- TRUE
    row_names <- FALSE
  }

  if (!is.matrix(x)) {
    x_name <- deparse(substitute(x))
    x <- univec2unimat(x, x_name)
  }

  periods <- get_periods(x)
  if (period_as_date)  {
    periods <- as.Date(periods)
  } else {
    periods <- as.character(periods)
  }

  lbls <- ts_labels(x)

  if (rowwise) {

    ret <- as.data.frame(t(x), ...)
    colnames(ret) <- periods

    if (!is.null(lbls)) {
      ret <- cbind(label = lbls, ret, stringsAsFactors = FALSE)
    }

    if (!row_names) {
      rownames(ret) <- NULL
      ret <- cbind(name = colnames(x), ret, stringsAsFactors = FALSE)
    }

  } else {

    # columnwise

    ret <- as.data.frame.ts(x, ...)

    if (!is.null(lbls)) {
      # Add labels to the data frame, using Rcpp function
      # add_labels_df, which adds labels in place. This function is very
      # slow when implemented in R.
      invisible(add_labels_df(ret, unname(lbls)))
    }

    if (row_names) {
      rownames(ret) <- periods
    } else {
      ret <- cbind(period = periods, ret, stringsAsFactors = FALSE)
    }
  }

  if (long) {

    ret <- pivot_longer(ret, cols = where(is.numeric), names_to = "period") |>
      as.data.frame()

    if (period_as_date) {
      # Column 'period' now contains the periods as texts (because column names
      # are always character variables. Restore the original periods, by using
      # names Date vector.
      period_dict <- periods
      names(period_dict) <- as.character(periods)
      ret$period <- period_dict[ret$period]
    }
  }

  return(ret)
}
