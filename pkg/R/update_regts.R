
#' Updates a multivariate timeseries with another multivariate timeseries
#' objects
#'
#' This function can be used to update a multivariate regts object with
#' another multivariate regts object.
#' The result is the updated multivariate regts.

#'
#' @details
#' This function calculates the difference between common columns
#' of two multivariate timeseries objects \code{x1} and \code{x2}.
#' The two timeseries must have the same frequency, but may have a different
#' period range. The difference is computed for the union of the two
#' period ranges (for the non-overlapping periods the result will be
#' \code{NA}). Two \code{NA} values are considered to be equal.
#'
#' @export
#' @param x1 the first timeseries (a multivariate \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param x2 the second timeseries (a multivariate \code{regts} or \code{ts} object).
#' @param tol difference tolerance (by default zero). Differences smaller
#' than tol are ignored.
#' @param fun function to compute differences. This function should accept
#' two arguments (two numbers) for which the difference is computed.
#' By default the absolute difference is computed. A useful function for
#' computing differences is \code{\link{cvgdif}}, which computes relative differences
#' if the absolute value of \code{x2} is larger than 1.
#' @return a list with the following components

#' @examples
#' library(regts)
#'
#'# create two timeseries x1 and x2
#' x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2002",
#'             names = c("a", "b", "c"))
#' x2 <- regts(matrix(data = rep(2), nc = 3), period = "2000/2001",
#'             names = c("a", "c", "d"))
#'x1 <- regts(matrix(data = rep(1:127), nc = 3), start = "2000",
#'            names = c("a", "b", "c"))
#'x2 <- x1 + 0.001
#'colnames(x2) <- c("a", "b", "d")
#'
#' @seealso
#'\code{\link{regts}}
#'
tsupdate <- function(x1, x2, method = c("tsupd", "tsupdna", "replace", "tsupdval")) {

  if (!is.mts(x1)) {
    stop(paste0("Argument x1 (", deparse(substitute(x1))),
               ") is not a multivariate timeseries")
  }
  if (!is.mts(x2)) {
    stop(paste0("Argument x2 (", deparse(substitute(x2))),
               ") is not a multivariate timeseries")
  }

  x1 <- as.regts(x1)
  x2 <- as.regts(x2)
  if (frequency(x1) != frequency(x2)) {
    series_name1 <- deparse(substitute(x1))
    series_name2 <- deparse(substitute(x2))
    stop(paste0("Arguments x1 and x2 (", series_name1, "and", series_name2,
               "have different frequencies"))
  }

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

  common_names   <- intersect(names1, names2)
  missing_names1 <- setdiff(names2, names1)
  missing_names2 <- setdiff(names1, names2)
  print(missing_names1)
  print(missing_names2)

  # start update
  if (method == "replace"){
    return(x2)
  }

  p1 <- get_period_range(x1)
  p2 <- get_period_range(x2)

  update <- calculate_update(common_names, x1, x2, p1, p2, method)

  if (!is.null(update)) {
    updnames <- colnames(update)
  } else {
    updnames <- character(0)
  }

	# add variables that appear only in one object
#  print(update)
#  print(x1)
#  print(x2[, missing_names1])


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
calculate_update <- function(common_names, x1, x2, p1, p2, method) {

  var_count <- length(common_names)
  if (var_count == 0) {
      return (x1)
  }

  # Align the two timeseries objects using the union of their times.
  punion <- c(min(p1[1], p2[1]), max(p1[2], p2[2]), p1[3])

  xx1 <- x1[, common_names, drop = FALSE]
  xx2 <- x2[, common_names, drop = FALSE]
  # extend common regts to union of periods, fill non overlapping periods with NA
  xx1 <- window_regts(xx1, punion)
  xx2 <- window_regts(xx2, punion)

  if (method == "tsupd") {
    xx1[p2, common_names] <- xx2[p2, common_names]
  }
  else if (method == "tsupdna") {
    !is.na(xx1[p2, common_names]) <- xx2[p2, common_names]
  }
  else if (method == "tsupdval") {
    xx1[p2, common_names] <- !is.na(xx2[p2, common_names])
  }

  return(xx1)

  # If xx1 and xx2 are both NA, then replace NA with 0.
  # Two NA values are always considered equal.
  # both_na <- is.na(xx1) & is.na(xx2)
  #  xx1[both_na] <- 0
  #  xx2[both_na] <- 0

}





