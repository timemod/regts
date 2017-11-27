#' Update a multivariate timeseries with another multivariate timeseries
#' object
#'
#' This function can be used to update a multivariate (reg)ts object with
#' another multivariate (reg)ts object.
#' The result is an updated multivariate \code{\link{regts}} object.
#'
#' @details
#' The timeseries can be updated in four different ways:
#'
#' \code{upd} the first timeseries are updated with the second timeseries.
#' The two timeseries must have the same frequency, but may have a different
#' period range. The update is computed for the union of the two period ranges
#' (for the non-overlapping periods the result will be \code{NA}).
#'
#' \code{updna} if method \code{updna} is selected instead of \code{upd},
#' only NA values in the first timeseries will be updated
#'
#' \code{updval} if method \code{updval} is selected instead of \code{upd},
#' the values in the first timeseries are only replaced with valid (i.e. non-NA)
#' values from the second timeseries.
#'
#' \code{replace} if method \code{replace} is selected the first
#' timeseries is replaced by the second timeseries
#'
#' The first three update methods are only employed at the common columns in
#' both timeseries.
#' The non overlapping columns in both timeseries are added to the result.
#'
#' @param x1 the first timeseries (a multivariate \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param x2 the second timeseries (a multivariate \code{regts} or \code{ts} object).
#' @param method four different ways to update the timeseries.
#' By default the timeseries are updated. This behaviour can be changed by
#' using one of the other methods. See details.
#' @return an updated multivariate \code{\link{regts}} object.
#'
#' @examples
#' library(regts)
#'
#' x1 <- regts(matrix(data = rep(1:9), nc = 3), period = "2000/2002",
#'             names = c("a", "b", "c"))
#' x2 <- regts(matrix(data = rep(10:15), nc = 3), period = "2000/2001",
#'             names = c("a", "c", "d"))
#' update_ts(x1, x2, method = "upd")
#'
#' @seealso
#'\code{\link{regts}}
#'
#' @export
update_ts <- function(x1, x2, method = c("upd", "updna", "updval", "replace")) {

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

  # for method updval first remove all leading and trailing rows and all
  # columns from x2 with only NA
  if (method == "updval") {
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

  # if replace then ready
  if (method == "replace") {
    return (update)
  }

  # update result with non common names
  update[p2, missing_names1] <- x2[p2, missing_names1]
  update[p1, missing_names2] <- x1[p1, missing_names2]

  if (length(missing_names1) > 0) {
    # add labels of missing_names1 in x2 to the result
    lbls <- ts_labels(x2)
    if (!is.null(lbls)) {
      lbls <- lbls[missing_names1]
      update <- update_ts_labels(update, lbls)
    }
  }
  if (length(missing_names2) > 0) {
    # add labels of missing_names2 in x1 to the result
    lbls <- ts_labels(x1)
    if (!is.null(lbls)) {
      lbls <- lbls[missing_names2]
      update <- update_ts_labels(update, lbls)
    }
  }

  # sort columns of update
  if (!is.null(update)) {
    update <- update[, sort(colnames(update)), drop = FALSE]
  }

  return (update)
}


# In case of methods upd, updna and updval calculate the update for the common
# columns in x1 and x2 and return a regts for the union of periods
# In case of replace just return x2
calculate_update <- function(x1, x2, common_names, p1, p2, method) {

  if (method == "replace") {
    return(x2)
  }

  var_count <- length(common_names)
  if (var_count == 0) {
    return(x1)
  }

  # extend common regts to union of periods, fill non overlapping periods with NA
  p_union <- range_union(p1, p2)
  xx1 <- x1[p_union, common_names, drop = FALSE]
  xx2 <- x2[p_union, common_names, drop = FALSE]

  if (method == "upd") {
    xx1[p2, ] <- xx2[p2, ]

  } else if (method == "updna") {
    na_xx1 <- is.na(xx1)
    xx1[na_xx1] <- xx2[na_xx1]

  } else if (method == "updval") {
    not_na_xx2 <- !is.na(xx2)
    xx1[not_na_xx2] <- xx2[not_na_xx2]
  }

  return(xx1)
}






