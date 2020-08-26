#' Update a timeseries with another timeseries object
#'
#' This function can be used to update, replace or extend a \code{(reg)ts}
#' object with another \code{(reg)ts} object.
#' The result is an updated \code{\link{regts}} object.
#'
#' @details
#' The two timeseries must have the same frequency, but may have a different
#' period range.
#' The common columns in the timeseries can be updated in four different ways:
#'
#' \code{upd} the first timeseries are updated with the second timeseries for the
#' total period range of the second timeseries. Outside this period the values
#' in the first timeseries do not change.
#'
#' \code{updna} if method \code{updna} is selected instead of \code{upd},
#' only NA values in the first timeseries will be updated
#'
#' \code{updval} if method \code{updval} is selected instead of \code{upd},
#' the values in the first timeseries are only replaced with valid (i.e. non-NA)
#' values from the second timeseries.
#'
#' \code{replace} like method \code{upd}, the values in the first
#' timeseries are replaced by the values in the second timeseries for the total
#' period range of these second timeseries.
#' Outside this period the values in the first timeseries will become
#' \code{NA}.
#'
#' By default, columns only present in one of the two timeseries objects
#' are added to the result.
#' The result columns are the columns of the first series, supplemented
#' with the columns of the second series.
#' If parameter \code{join_second} is \code{FALSE} then the remaining columns
#' of the second timeseries are *not* added to the result. Thus the result
#' series has the same columns as the first series.
#'
#' The period range of the result is the union of the period ranges of the
#' first and second timeseries, except for the \code{updval} method.
#' For this method the result period range is the union of the period ranges
#' of the first timeseries and the timeseries obtained by applying function
#' \code{na_trim} to the second timeseries.
#'
#' @param x1 the first timeseries (a \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param x2 the second timeseries (a \code{regts} or \code{ts} object).
#' @param method four different ways to update the timeseries.
#' By default the timeseries are updated. This behaviour can be changed by
#' using one of the other methods. See details.
#' @param join_second A logical (default \code{TRUE}) indicating whether columns
#' present in the second timeseries but missing in the first timeseries should
#' be added to the result.
#' @return an updated \code{\link{regts}} object.
#'
#' @examples
#' x1 <- regts(matrix(data = rep(1:9), nc = 3), period = "2000/2002",
#'             names = c("a", "b", "c"))
#' x2 <- regts(matrix(data = rep(10:15), nc = 3), period = "2000/2001",
#'             names = c("a", "c", "d"))
#' update_ts(x1, x2, method = "upd")
#'
#' @seealso
#'\code{\link{regts}} and \code{\link{join_ts}}
#'
#' @export
update_ts <- function(x1, x2, method = c("upd", "updna", "updval", "replace"),
                      join_second = TRUE) {

  series_name1 <- deparse(substitute(x1))
  series_name2 <- deparse(substitute(x2))

  method <- match.arg(method)

  # Use function inherits instead of is.ts to check if x1 is a timeseries.
  # is.ts returns FALSE if x1 is a timeseries with 0 columns
  if (!inherits(x1, "ts")) {
    stop(paste0("Argument x1 (", series_name1,
                ") is not a timeseries"))
  }
  if (!inherits(x2, "ts")) {
    stop(paste0("Argument x2 (", series_name2,
               ") is not a timeseries"))
  }

  if (frequency(x1) != frequency(x2)) {
    stop(paste0("Timeseries x1 and x2 (", series_name1, " and ", series_name2,
               ") have different frequencies"))
  }

  x1 <- as.regts(x1)
  x2 <- as.regts(x2)

  names1 <- colnames(x1)
  names2 <- colnames(x2)

  # create colnames if x1 or x2 does not have colnames
  if (is.null(names1) && NCOL(x1) > 0) {
    if (is.matrix(x1)) {
      names1 <- paste("column", 1 : ncol(x1))
    } else {
      # adapt (vector) timeseries: use timeseries name and give matrix dimension
      names1 <- series_name1
      dim(x1) <- c(length(x1), 1)
    }
    colnames(x1) <- names1
  }

  if (is.null(names2) && NCOL(x2) > 0) {
    if (is.matrix(x2)) {
      names2 <- paste("column", 1 : ncol(x2))
    } else {
      # adapt (vector) timeseries: use timeseries name and give matrix dimension
      names2 <- series_name2
      dim(x2) <- c(length(x2), 1)
    }
    colnames(x2) <- names2
  }

  # for method updval first remove all leading and trailing rows and all
  # columns from x2 with only NA
  if (method == "updval" && ncol(x2) > 0) {
    x2 <- na_trim(x2)
    if (!is.null(x2)) {
      x2 <- remove_na_columns(x2)
      names2 <- colnames(x2)
    } else {
      return(x1)
    }
  }

  if (ncol(x1) == 0) return(x2)
  if (ncol(x2) == 0) return(x1)

  common_names  <- intersect(names1, names2)
  extra_names2  <- setdiff(names2, names1)

  p1 <- get_period_range(x1)
  p2 <- get_period_range(x2)

  update <- calculate_update(x1, x2, common_names, p1, p2, method)

  # if join_second = TRUE update result with extra names in second series
  if (join_second && length(extra_names2) > 0) {
    update[p2, extra_names2] <- x2[p2, extra_names2]

    # add labels of missing_names1 in x2 to the result
    lbls <- ts_labels(x2)
    if (!is.null(lbls)) {
      lbls <- lbls[extra_names2]
      update <- update_ts_labels(update, lbls)
    }
  }

  return (update)
}


# Calculate the update for the common columns in x1 and x2 and return a regts
# for the union of periods
calculate_update <- function(x1, x2, common_names, p1, p2, method) {

  var_count <- length(common_names)
  if (var_count == 0) {
    return(x1)
  }

  # extend common regts to union of periods, fill non overlapping periods with NA
  p_union <- range_union(p1, p2)
  xx1 <- x1[p_union, common_names, drop = FALSE]
  xx2 <- x2[p_union, common_names, drop = FALSE]

  if (method == "upd" || method == "replace") {
    if (method == "replace") xx1[] <- NA
    xx1[p2, ] <- xx2[p2, ]

  } else if (method == "updna") {
    na_xx1 <- is.na(xx1)
    xx1[na_xx1] <- xx2[na_xx1]

  } else if (method == "updval") {
    not_na_xx2 <- !is.na(xx2)
    xx1[not_na_xx2] <- xx2[not_na_xx2]
  }

  x1[p_union, common_names] <- xx1[p_union, common_names]

  return(x1)
}






