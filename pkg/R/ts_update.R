#' Updates a multivariate timeseries with another multivariate timeseries
#' object
#'
#' This function can be used to update a multivariate (reg)ts object with
#' another multivariate (reg)ts object.
#' The result is an updated multivariate regts object.
#'
#' @details
#' The timeseries can be updated in three different ways:
#'
#' \code{tsupd} the first timeseries are updated with the second timeseries.
#' The two timeseries must have the same frequency, but may have a different
#' period range. The update is computed for the union of the two period ranges
#' (for the non-overlapping periods the result will be \code{NA}).
#'
#' \code{tsupdna} if method \code{tsupdna} is selected instead of \code{tsupd},
#' only NA values in the first timeseries will be updated
#'
#' \code{tsupdval} if method \code{tsupdval} is selected instead of \code{tsupd},
#' the values in the first timeseries are only replaced with valid (i.e. non-NA)
#' values from the second timeseries.
#'
#' These methods are only employed at the common columns in both timeseries.
#' The non overlapping columns in both timeseries are added to the result.
#'
#' @param x1 the first timeseries (a multivariate \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param x2 the second timeseries (a multivariate \code{regts} or \code{ts} object).
#' @param method three different ways to update the timeseries.
#' By default the timeseries are updated. This behaviour can be changed by
#' using one of the other methods. See details.
#' @return an updated multivariate regts object.
#'
#' @examples
#' library(regts)
#'
#' x1 <- regts(matrix(data = rep(1:9), nc = 3), period = "2000/2002",
#'             names = c("a", "b", "c"))
#' x2 <- regts(matrix(data = rep(10:15), nc = 3), period = "2000/2001",
#'             names = c("a", "c", "d"))
#' ts_update(x1, x2, method = "tsupd")
#'
#' @seealso
#'\code{\link{regts}}
#'
#' @export
ts_update <- function(x1, x2, method = c("tsupd", "tsupdna", "tsupdval")) {

  if (!is.mts(x1)) {
    stop(paste0("Argument x1 (", deparse(substitute(x1)),
               ") is not a multivariate timeseries"))
  }
  if (!is.mts(x2)) {
    stop(paste0("Argument x2 (", deparse(substitute(x2)),
               ") is not a multivariate timeseries"))
  }

  if (frequency(x1) != frequency(x2)) {
    series_name1 <- deparse(substitute(x1))
    series_name2 <- deparse(substitute(x2))
    stop(paste0("Timeseries x1 and x2 (", series_name1, " and ", series_name2,
               ") have different frequencies"))
  }

  method <- match.arg(method)

  x1 <- as.regts(x1)
  x2 <- as.regts(x2)

  names1 <- colnames(x1)
  names2 <- colnames(x2)

  # create colnames if x1 or x2 does not have colnames
  if (is.null(names1)) {
    names1 <- paste("column", 1 : ncol(x1))
    colnames(x1) <- names1
  }
  if (is.null(names2)) {
    names2 <- paste("column", 1 : ncol(x2))
    colnames(x2) <- names2
  }

  # for method tsupdval first remove all leading and trailing rows and all
  # columns from x2 with only NA
  if (method == "tsupdval") {
    x2 <- na_trim(x2)
    x2 <- remove_na_columns(x2)
    names2 <- colnames(x2)
  }

  common_names   <- intersect(names1, names2)
  missing_names1 <- setdiff(names2, names1)
  missing_names2 <- setdiff(names1, names2)

  p1 <- get_period_range(x1)
  p2 <- get_period_range(x2)

  update <- calculate_update(x1, x2, common_names, p1, p2, method)

  # update result with non common names
  update[p2, missing_names1] <- x2[p2, missing_names1]
  update[p1, missing_names2] <- x1[p1, missing_names2]

  # sort columns of update
  if (!is.null(update)) {
    update <- update[, sort(colnames(update)), drop = FALSE]
  }

  return (update)
}


# Calculate the update for the common columns in x1 and x2,
# and return a regts for the union of periods
calculate_update <- function(x1, x2, common_names, p1, p2, method) {

  var_count <- length(common_names)
  if (var_count == 0) {
      return (x1)
  }

  xx1 <- x1[, common_names, drop = FALSE]
  xx2 <- x2[, common_names, drop = FALSE]

  # Align the two timeseries objects using the union of their times.
  punion <- c(min(p1[1], p2[1]), max(p1[2], p2[2]), p1[3])

  # extend common regts to union of periods, fill non overlapping periods with NA
  xx1 <- window_regts(xx1, punion)
  xx2 <- window_regts(xx2, punion)

  if (method == "tsupd") {
    xx1[p2, ] <- xx2[p2, ]

  } else if (method == "tsupdna") {
    na_xx1 <- is.na(xx1)
    xx1[na_xx1] <- xx2[na_xx1]

  } else if (method == "tsupdval") {
    not_na_xx2 <- !is.na(xx2)
    xx1[not_na_xx2] <- xx2[not_na_xx2]
  }

  return(xx1)
}






