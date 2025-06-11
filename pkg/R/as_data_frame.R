#' Convert a \code{\link{regts}} to a \code{\link[base]{data.frame}}
#'
#' @details

#' Three different format for the data frame are possbile.
#'
#' **1. columnwise (default)**
#'
#' This is the default format. There is a column for each variable. The first
#' column (by default named `period`) will contain the periods, as shown below:
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
#' #'```
#' name   label       2022  2023
#' a      Variable a     1     2
#' b      Variable b    10    20
#' ```
# The first column, by default  named `name`, will contain
# the variable names. If the timeseries has labels, the  second column,
# by default named `label`, contains the labels.
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
#' If the timeseries has no labels, the `label` column is missing.
#'
#' @param x a \code{\link{regts}}
#' @param format A character specifying the format (see Details): `"rowwise"',
#' `"columnwise"' or `"long"`.`
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
#' @param period_col The name of the column with periods (default `"period"`),
#' used for the columnwise or long format.
#' @param name_col   The name of the column with variable names
#' (default `"name"`), used for the rowwise or long format.
#' @param label_col  The name of the column with labels of the variables
#' (default `"label"`) used for the rowwise or long format. If the timeseries
#' does not have labels then this argument is ignored.
#' @param value_col  The name of the columns with values (default `"value"`)
#' for the long format.
#' @param ... additional arguments to be passed to methods.
#' @return A \code{\link[base]{data.frame}}
#' @name as.data.frame
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect where
#' @export
#' @examples
#' ts <- regts(matrix(1:4, ncol = 2) , start = "2015Q3", names = c("a", "b"),
#'            labels = c("Timeseries a", "Timeseries b"))
#' print(as_data_frame(ts))
#'
#' #' print(as_data_frame(ts, format = "long))
#' @export
as_data_frame <- function(x, ...) {
  UseMethod("as_data_frame")
}

#' @export
as_data_frame.regts <- function(x, ...,
                                format = c("columnwise", "rowwise", "long"),
                                period_as_date = FALSE,
                                period_col = "period",
                                name_col   = "name",
                                label_col  = "label",
                                value_col  = "value") {

  format <- match.arg(format)

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

  if (format %in% c("rowwise", "long")) {

    ret <- as.data.frame(t(x), ...)
    colnames(ret) <- periods

    if (!is.null(lbls)) {
      ret <- cbind(label = lbls, ret, stringsAsFactors = FALSE)
      colnames(ret)[1] <- label_col
    }

    ret <- cbind(name = colnames(x), ret, stringsAsFactors = FALSE)
    colnames(ret)[1] <- name_col

  } else {

    # columnwise

    ret <- as.data.frame.ts(x, ...)

    if (!is.null(lbls)) {
      # Add labels to the data frame, using Rcpp function
      # add_labels_df, which adds labels in place. This function is very
      # slow when implemented in R.
      invisible(add_labels_df(ret, unname(lbls)))
    }

    ret <- cbind(period = periods, ret, stringsAsFactors = FALSE)
    colnames(ret)[1] <- period_col
  }

  if (format == "long") {

    ret <- pivot_longer(ret, cols = where(is.numeric), names_to = "period") |>
      as.data.frame()

    if (period_as_date) {
      # Column 'period' now contains the periods as texts (because column names
      # are always character variables). Replace then by the original data
      # objects, by using a named vector with dates (the names are the dates
      # as text).
      period_dict <- periods
      names(period_dict) <- as.character(periods)
      ret$period <- period_dict[ret$period]
    }
  }

  rownames(ret) <- NULL

  return(ret)
}
