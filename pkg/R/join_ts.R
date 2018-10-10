#' Join timeseries object with different but overlapping period range
#'
#' This function creates a new timeseries from two (partially) overlapping
#' timeseries with the same frequency.
#' The second timeseries must contain the most recent data.
#' When determining the overlap period also NA values are considered.
#'
#' All observations from the first timeseries are scaled in such a way that
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
#' In case of multivariate regts only the common columns are joined. For each
#' common timeseries a check is done whether an overlapping period exists
#' (consdiering NA values).
#' The non overlapping columns in both timeseries are added to the result.
#' If both input timeseries are vectors (no column names), the result is also a vector.
#'
#' @param old the first timeseries (a \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param new the second timeseries (a \code{regts} or \code{ts} object).
#' @param method two different ways to join the timeseries.
#' By default the timeseries are joined multiplicatively. This behaviour can be
#' changed by using method \code{add}.
#' @return a \code{\link{regts}} object.
#'
#' @examples
#' x1 <- regts(rnorm(40), start = "2012q1")
#' x2 <- regts(rnorm(30), start = "2018q4")
#' res <- join_ts(x1, x2)
#'
#' x_old <- regts(matrix(data = rep(10:15), nc = 3), period = "2000/2001",
#'             names = c("a", "c", "d"))
#' x_new <- regts(matrix(data = rep(1:9), nc = 3), period = "2000/2002",
#'             names = c("a", "b", "c"))

#' join_ts(x_old, x_new, method = "add")
#'
#' @seealso '\code{\link{regts}} and \code{\link{update_ts}}
#'
#' @export
join_ts <- function(old, new, method = c("mult", "add")) {

  x_old <- old
  x_new <- new

  series_name_new <- deparse(substitute(new))
  series_name_old <- deparse(substitute(old))

  method <- match.arg(method)

  if (!is.ts(x_new)) {
    stop(paste0("Argument new (", series_name_new,
                ") is not a timeseries"))
  }
  if (!is.ts(x_old)) {
    stop(paste0("Argument old (", series_name_old,
               ") is not a timeseries"))
  }

  if (frequency(x_new) != frequency(x_old)) {
    stop(paste0("Timeseries old and new (", series_name_new, " and ",
                series_name_old, ") have different frequencies"))
  }

  x_new <- as.regts(x_new)
  x_old <- as.regts(x_old)

  names_new <- colnames(x_new)
  names_old <- colnames(x_old)

  vector_new <- FALSE
  vector_old <- FALSE

  # create colnames if x_new or x_old are vectors and do not have column names
  if (is.null(names_new)) {
    if (is.matrix(x_new)) {
      names_new <- paste("column", 1 : ncol(x_new))
    } else {
      # adapt (vector) timeseries: use timeseries names and give matrix dimension
      names_new <- paste(series_name_old, series_name_new, sep = "_")
      dim(x_new) <- c(length(x_new), 1)
      vector_new <- TRUE

    }
    colnames(x_new) <- names_new
  }
  if (is.null(names_old)) {
    if (is.matrix(x_old)) {
      names_old <- paste("column", 1 : ncol(x_old))
    } else {
      # adapt (vector) timeseries: use timeseries names and give matrix dimension
      names_old <- paste(series_name_old, series_name_new, sep = "_")
      dim(x_old) <- c(length(x_old), 1)
      vector_old <- TRUE
    }
    colnames(x_old) <- names_old
  }

  if (vector_new != vector_old){
    stop("A combination of a vector and a multivariate (reg)ts is not possible")
  }

  common_names <- intersect(names_new, names_old)
  missing_names_new <- setdiff(names_old, names_new)
  missing_names_old <- setdiff(names_new, names_old)

  if (length(common_names) == 0) {
    warning("No common names in two timeseries, new timeseries is returned!")
    return(x_new)
  }

  p_new <- get_period_range(x_new)
  p_old <- get_period_range(x_old)

  # check periods (must be overlapping)
  sp_new <- start_period(p_new)
  sp_old <- start_period(p_old)
  ep_new <- end_period(p_new)
  ep_old <- end_period(p_old)

  if (sp_new < sp_old || ep_new < ep_old){
    stop("Timeseries are in wrong order, old series should start before new!")
  }
  if (sp_new > ep_old){stop("Timeseries have no overlap!")}

  distance <- sp_new - sp_old

  join <- calculate_join(x_old, x_new, common_names, p_old, p_new, method,
                         distance, vector_new)

  # update for multivariate timeseries
  if (!vector_new){
    # update result with non common names
    if (length(missing_names_new) > 0) {
      join[p_old, missing_names_new] <- x_old[p_old, missing_names_new]

      # add labels of missing_names_new in x_old to the result
      lbls <- ts_labels(x_old)
      if (!is.null(lbls)) {
        lbls <- lbls[missing_names_new]
        join <- update_ts_labels(join, lbls)
      }
    }
    if (length(missing_names_old) > 0) {
      join[p_new, missing_names_old] <- x_new[p_new, missing_names_old]

      # add labels of missing_names_old in x_new to the result
      lbls <- ts_labels(x_new)
      if (!is.null(lbls)) {
        lbls <- lbls[missing_names_old]
        join <- update_ts_labels(join, lbls)
      }
    }

    # sort columns of join
    if (!is.null(join)) {
      join <- join[, order(colnames(join)), drop = FALSE]
    }
  }

  return (join)
}

# Schematic overview of periods
# -----------------------------------------------------------------------------
# new timeseries                    [      |                           ]
#                                   sp_new f_new                       ep_new
#
# indices in matrix m_new                  1       l_old-d (=l_new)
#
# old timeseries           [                       |      ]
#                          sp_old                  l_old  ep_old
#
# indices in matrix m_old  1               f_new+d (=f_old)
#
# indices in result        [               |       |                    ]
#                          1        1+d    f_old   l_old                nper
#
#                          [--fill_prd_1-- |--fill_prd_2----------------]
#
# f_new is first not NA in new timeseries
# l_old is last not NA in old timeseries
#
# o, overlapping period   : [f_old : f_new]  = [l_old : l_new]
# d, distance startperiods: sp_new - sp_old

# Result series contain
#   original values of new timeseries for overlap period till end of new ts
#   calculated values for period starting in first period old ts till overlap

# Result series is a multivariate regts or (if both inputs are vectors) a vector.

# In calculate_join an extra check is done on NA values at the edges of the
# overlap period. Based on this new overlap the filling periods are calculated.


calculate_join <- function(x_old, x_new, common_names, p_old, p_new, method,
                           distance, vector){
  # Calculate the joined timeseries for the common columns in x_new and x_old
  # and return a regts for the union of periods
  # use matrix to fasten calculations (same code with regts functions is much slower)

  # create result matrix with NA values for whole period and all common names
  p <- range_union(p_old, p_new)
  nper <- nperiod(p)
  ncol <- length(common_names)
  ret_matrix <- matrix(nrow = nper, ncol = ncol)

  m_new <- x_new[, common_names, drop = FALSE]
  m_old <- x_old[, common_names, drop = FALSE]

  for (ix in 1:ncol){

    # trim individual series to see if there is still an overlapping period
    # f_new is first not NA in new series, l_old is last not NA in old series
    f_new <- Position(function(x){x}, !is.na(m_new[, ix]))
    f_old <- f_new + distance
    l_old  <- Position(function(x){x}, !is.na(m_old[, ix]), right = TRUE)
    l_new  <- l_old - distance

    if (f_new > l_new){
      if (vector){
        stop(paste("Combination of old and new timeseries has no valid values
                   in overlapping period!"))
      } else{
        name <- common_names[ix]
        stop(paste("In old and new series, combination of timeseries", name,
             "has no valid values in overlapping period!"))
      }
    }
    # overlap ranges in matrices have the same lengths but are shifted
    rng_m_new <- f_new : l_new
    rng_m_old <- f_old : l_old

    mn_new <- mean( m_new[rng_m_new, ix] )
    mn_old <- mean( m_old[rng_m_old, ix] )

    fill_prd_1 <- 1 : (f_old - 1)
    fill_prd_2 <- f_old : nper

    if (method == "mult"){
      factor <- mn_new/mn_old
      ret_matrix[fill_prd_1, ix] <- m_old[fill_prd_1, ix] * factor

    } else{
      factor <- mn_new - mn_old
      ret_matrix[fill_prd_1, ix] <- m_old[fill_prd_1, ix] + factor
    }

    ret_matrix[fill_prd_2, ix] <- m_new[(fill_prd_2 - distance), ix]
  }

  if (vector){
    ret <- regts(as.vector(ret_matrix), period = p)
  } else {
    ret <- regts(ret_matrix, period = p, names = common_names)
  }
  return(ret)
}
