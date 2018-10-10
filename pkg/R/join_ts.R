#' Join a timeseries object with another timeseries object
#'
#' This function can be used to join a \code{(reg)ts}
#' object with another \code{(reg)ts} object.
#' The result is a joined \code{\link{regts}} object.
#'
#' @details
#' This function creates a new timeseries from two (partially) overlapping
#' timeseries. The two timeseries must have the same frequency.
#' The first timeseries must contain the most recent data.
#' All observations from the second timeseries are scaled in such a way that
#' the overlapping observations from the two timeseries have the same value
#' (on average). Scaling methods are:
#'
#' \code{mult} multiplicative joining (default)
#'
#' \code{add} additive joining
#'
#' The period range of the result is the union of the period ranges of the
#' first and second timeseries.
#'
#' In case of multivariate regts only the common columns are joined.
#' The non overlapping columns in both timeseries are added to the result.
#' If both input timeseries are vectors (no column names), the result is also a vector.
#'
#' @param x1 the first timeseries (a \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param x2 the second timeseries (a \code{regts} or \code{ts} object).
#' @param method two different ways to join the timeseries.
#' By default the timeseries are joined multiplicatively. This behaviour can be
#' changed by using method \code{add}.
#' @return a \code{\link{regts}} object.
#'
#' @examples
#' x1 <- regts(matrix(data = rep(1:9), nc = 3), period = "2000/2002",
#'             names = c("a", "b", "c"))
#' x2 <- regts(matrix(data = rep(10:15), nc = 3), period = "2000/2001",
#'             names = c("a", "c", "d"))
#' join_ts(x1, x2, method = "add", period = "first")
#'
#' @seealso
#'\code{\link{regts}}
#'
#' @export
join_ts <- function(x1, x2, method = c("mult", "add")) {

  series_name1 <- deparse(substitute(x1))
  series_name2 <- deparse(substitute(x2))

  method <- match.arg(method)

  if (!is.ts(x1)) {
    stop(paste0("Argument x1 (", series_name1,
                ") is not a timeseries"))
  }
  if (!is.ts(x2)) {
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

  vector1 <- FALSE
  vector2 <- FALSE

  # create colnames if x1 or x2 are vectors and do not have colnames
  if (is.null(names1)) {
    if (is.matrix(x1)) {
      names1 <- paste("column", 1 : ncol(x1))
    } else {
      # adapt (vector) timeseries: use timeseries names and give matrix dimension
      names1 <- paste(series_name1, series_name2, sep = "_")
      dim(x1) <- c(length(x1), 1)
      vector1 <- TRUE
    }
    colnames(x1) <- names1
  }
  if (is.null(names2)) {
    if (is.matrix(x2)) {
      names2 <- paste("column", 1 : ncol(x2))
    } else {
      # adapt (vector) timeseries: use timeseries name and give matrix dimension
      names2 <- paste(series_name1, series_name2, sep = "_")
      dim(x2) <- c(length(x2), 1)
      vector2 <- TRUE
    }
    colnames(x2) <- names2
  }

  if (vector1 != vector2){
    stop("Both timeseries must be vectors or both should have column names")
  }

  common_names <- intersect(names1, names2)
  missing_names1 <- setdiff(names2, names1)
  missing_names2 <- setdiff(names1, names2)

  if (length(common_names) == 0) {
    warning("No common names in two timeseries, first timeseries is returned!")
    return(x1)
  }

  p1 <- get_period_range(x1)
  p2 <- get_period_range(x2)

  # check periods (must be overlapping)
  sp1 <- start_period(p1)
  sp2 <- start_period(p2)
  ep1 <- end_period(p1)
  ep2 <- end_period(p2)
  if (ep1 < sp1){stop("End period first timeseries must be after start period")}
  if (ep2 < sp2){stop("End period second timeseries must be after start period")}
  if (sp1 < sp2 || ep1 < ep2){stop("Timeseries are in wrong order!")}
  if (sp1 > ep2){stop("Timeseries have no overlap!")}

  distance <- sp1 - sp2

  join <- calculate_join(x1, x2, common_names, p1, p2, method, distance, vector1)

  # update for multivariate timeseries
  if (!vector1){
    # update result with non common names
    if (length(missing_names1) > 0) {
      join[p2, missing_names1] <- x2[p2, missing_names1]

      # add labels of missing_names1 in x2 to the result
      lbls <- ts_labels(x2)
      if (!is.null(lbls)) {
        lbls <- lbls[missing_names1]
        join <- update_ts_labels(join, lbls)
      }
    }
    if (length(missing_names2) > 0) {
      join[p1, missing_names2] <- x1[p1, missing_names2]

      # add labels of missing_names2 in x1 to the result
      lbls <- ts_labels(x1)
      if (!is.null(lbls)) {
        lbls <- lbls[missing_names2]
        join <- update_ts_labels(join, lbls)
      }
    }

    # sort columns of join
    if (!is.null(join)) {
      join <- join[, sort(colnames(join)), drop = FALSE]
    }
  }

  return (join)
}

# Schematic overview of periods
# ---------------------------------------------------------
# first timeseries                  [     |        ]
#                                   sp1   s1       ep1
#
# indices in matrix m1              1     1+o
#
# second timeseries      [          |     ]
#                        sp2        s2    ep2
#
# indices in matrix m2   1          1+d
#
# indices in result      [          |     |        ]
#                        1          1+d   1+d+o    nper
#
# o, overlapping period   : s1 - s2 = ep2 - sp1
# d, distance startperiods: sp1 - sp2

# Result series contain
#   original values of first timeseries for overlap period till end of first ts
#   calculated values for period starting in first period second ts till overlap

# Result series is a multivariate regts or (if both inputs are vectors) a vector.

# In calculate_join an extra check is done on NA values at the edges of the
# overlap period. Based on the (new) overlap the filling periods are calculated.


calculate_join <- function(x1, x2, common_names, p1, p2, method, distance, vector){
  # Calculate the joined timeseries for the common columns in x1 and x2
  # and return a regts for the union of periods
  # use matrix to fasten calculations (same code with regts functions is much slower)

  # create result matrix with NA values for whole period and all common names
  p <- range_union(p1, p2)
  nper <- nperiod(p)
  ncol <- length(common_names)
  ret_matrix <- matrix(nrow = nper, ncol = ncol)

  m1 <- x1[, common_names, drop = FALSE]
  m2 <- x2[, common_names, drop = FALSE]

  for (ix in 1:ncol){

    # trim individual series to see if there is still an overlapping period
    # 1 is index in first series, 2 is index in second series
    first_not_NA1 <- Position(function(x){x}, !is.na(m1[, ix]))
    first_not_NA2 <- first_not_NA1 + distance
    last_not_NA2  <- Position(function(x){x}, !is.na(m2[, ix]), right = TRUE)
    last_not_NA1  <- last_not_NA2 - distance

    if (first_not_NA1 > last_not_NA1){
      name <- common_names[ix]
      stop(paste("Timeseries", name, "has no valid values in overlapping period!"))
    }
    # overlap ranges in matrices have the same lengths but are shifted
    rng_m1 <- first_not_NA1:last_not_NA1
    rng_m2 <- first_not_NA2:last_not_NA2

    fill_prd1 <- 1:(first_not_NA2 - 1)
    fill_prd2 <- first_not_NA2:nper

    mn1 <- mean(m1[rng_m1, ix])
    mn2 <- mean(m2[rng_m2, ix])

    if (method == "mult"){
      factor <- mn1/mn2
      ret_matrix[fill_prd1, ix] <- m2[fill_prd1, ix] * factor

    } else{
      factor <- mn1 - mn2
      ret_matrix[fill_prd1, ix] <- m2[fill_prd1, ix] + factor
    }

    ret_matrix[fill_prd2, ix] <- m1[(fill_prd2 - distance), ix]
  }

  if (vector){
    ret <- regts(as.vector(ret_matrix), period = p)
  } else {
    ret <- regts(ret_matrix, period = p, names = common_names)
  }
  return(ret)
}
