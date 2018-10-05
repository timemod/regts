#' Join a timeseries with another timeseries object
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
#' (on average). The scaling method can be either multplicative or additive.
#' There is an option for specifying how to use the overlapping observations.
#'
#' The common columns in the timeseries can be joined in two different ways:
#'
#' \code{mult} multiplicative joining
#'
#' \code{add} additive joining
#'
#' Argument \code{period} specifies which overlapping observations to use.
#' Specify
#' \code{first} to use first overlapping observation
#'
#' \code{last} to use last overlapping observation
#'
#' \code{whole} to use all overlapping observations
#'
#' #' The non overlapping columns in both timeseries are added to the result.
#'
#' The period range of the result is the union of the period ranges of the
#' first and second timeseries.
#'
#' @param x1 the first timeseries (a \code{\link{regts}} or
#'            \code{\link[stats]{ts}} object).
#' @param x2 the second timeseries (a \code{regts} or \code{ts} object).
#' @param method two different ways to join the timeseries.
#' By default the timeseries are joined multiplicatively. This behaviour can be changed by
#' using method \code{add}.
#' @param period three different ways to specify which overlapping observations
#' are used. By default all overlapping observations are used. See details for
#' other methods.
#' @return an updated \code{\link{regts}} object.
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

  # create colnames if x1 or x2 does not have colnames
  if (is.null(names1)) {
    if (is.matrix(x1)) {
      names1 <- paste("column", 1 : ncol(x1))
    } else {
      # adapt (vector) timeseries: use timeseries name and give matrix dimension
      names1 <- series_name1
      dim(x1) <- c(length(x1), 1)
    }
    colnames(x1) <- names1
  }
  if (is.null(names2)) {
    if (is.matrix(x2)) {
      names2 <- paste("column", 1 : ncol(x2))
    } else {
      # adapt (vector) timeseries: use timeseries name and give matrix dimension
      names2 <- series_name2
      dim(x2) <- c(length(x2), 1)
    }
    colnames(x2) <- names2
  }

  common_names   <- intersect(names1, names2)
  if (length(common_names) == 0) {
    warning("No common names in two timeseries, first timeseries is returned!")
    return(x1)
  }
  missing_names1 <- setdiff(names2, names1)
  missing_names2 <- setdiff(names1, names2)

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

  method <- "add"

  start <- sp2
  end <- ep1


  starttime <- Sys.time()
  join <- calculate_join(x1, x2, common_names, p1, p2, method, sp2, ep1)
  endtime <- Sys.time()
  Totaltime2 <- endtime - starttime
  cat(paste("Total time ", endtime - starttime, "\n"))

  # update result with non common names
  join[p2, missing_names1] <- x2[p2, missing_names1]
  join[p1, missing_names2] <- x1[p1, missing_names2]

  if (length(missing_names1) > 0) {
    # add labels of missing_names1 in x2 to the result
    lbls <- ts_labels(x2)
    if (!is.null(lbls)) {
      lbls <- lbls[missing_names1]
      join <- update_ts_labels(join, lbls)
    }
  }
  if (length(missing_names2) > 0) {
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

  return (join)
}

# Calculate the joined timeseries for the common columns in x1 and x2
# and return a regts for the union of periods
calculate_join <- function(x1, x2, common_names, p1, p2, method, start, end) {

  # create result regts with NA values for whole period and common names
  p <- range_union(p1, p2)
  ret <- regts(matrix(nrow = nperiod(p), ncol = length(common_names)),
               period = p, names = common_names)


  for (name in common_names){

    # trim individual series to see if there is still an overlapping period
#    xx1 <- na_trim(x1[, name])
#    xx2 <- na_trim(x2[, name])
#    pp1 <- get_period_range(xx1)
#    pp2 <- get_period_range(xx2)
#    rng <- range_intersect(pp1, pp2)

    first_not_NA <- Position(function(x){x}, !is.na(x1[, name]))
    pp1 <- period_range(start_period(p1) + first_not_NA - 1, end_period(p1) )

    last_not_NA  <- Position(function(x){x}, !is.na(x2[, name]), right = TRUE)
    pp2 <- period_range(start_period(p2), start_period(p2) + last_not_NA -1 )
    rng <- range_intersect(pp1, pp2)

    if (is.null(rng)){
      # only values of x1 are used, rest NA
      warning(paste("Common timeseries", name,
                    paste("have no valid values in overlapping period!\n",
                          "Only values of most recent timeseries are used.")))
    } else{

      fill_prd <- period_range(start, start_period(rng) - 1)

      m1 <- mean(x1[rng, name])
      m2 <- mean(x2[rng, name])

      if (method == "mult"){
        factor <- m1/m2
        ret[fill_prd, name] <- x2[fill_prd, name] * factor

      } else{
        factor <- m1 - m2
        ret[fill_prd, name] <- x2[fill_prd, name] + factor
      }
    }

    ret[pp1, name] <- x1[pp1, name]
  }

  return(ret)
}

x1 <- regts(matrix(c(1,2,3,4,5,
                     NA,2,3,4,5,
                     NA,2,3,4,5), ncol = 3), start = "2004", names = c("a","b","c"))
x2 <- regts(matrix(c(9,10,11,12,13,
                     9,10,11,12,13,
                     9,10,11,12,NA), ncol = 3), start = "2002", names = c("a","b","c"))

nr <- 10
names <- paste0("a",1:nr)
p1 <- period_range("2018m1", "2044m12")
x1 <- regts(matrix(rnorm(nperiod(p1)*nr),ncol = nr), start = start_period(p1),
            names = names)

p2 <- period_range("2000m1", "2010m12")
x2 <- regts(matrix(rnorm(nperiod(p2)*nr),ncol = nr), start = start_period(p2),
            names = names)
