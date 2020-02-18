#' Bind two or more timeseries
#'
#' Bind two or more timeseries objects with a common frequency.
#' By default, the period range of the result is the union of the period ranges
#' of the individual timeseries. The result is padded with \code{NA}s if
#' necessary. If argument \code{union} is false, then the period range
#' of the result is the intersection of the period ranges.
#'
#' @param  ... two or more univariate or multivariate timeseries,
#' or objects which can be coerced to timeseries
#' @param union if \code{TRUE}, then the period range of the result
#' is the union of the period ranges of the joined objects
#' (the result is padded with \code{NA}s if necessary). If \code{FALSE},
#' then the period range of the result is the intersection of the period ranges
#' of the joined objects.
#' @param suffixes Suffixes appended to the column names for all overlapping
#' columns. This argument is obligatory if the timeseries have overlapping
#' column names. Length suffixes must be equal to the number of joined timeseries
#' or objects.
#' @seealso \code{\link{as.list}}
#' @importFrom stats ts.union ts.intersect
#' @examples
#' a <- regts(1:5, start = "2011Q1")
#' b <- regts(matrix(11:15, nc = 1), start = "2011Q2")
#' cbind(a, b)
#' cbind(a, b, union = FALSE)
#' x1 <- regts(matrix(1:27, nc = 3), start = "2008Q4", names = c("a", "b", "c"))
#' x2 <- regts(matrix(1:27, nc = 3), start = "2008Q4", names = c("a", "c", "d"))
#' cbind(x1, x2, suffixes = c("_1","_2"))
#' @name cbind
#' @export
cbind.regts <- function(..., union = TRUE, suffixes) {

  # construct names for the input arguments
  ts_names <- .get_ts_names(...)

  return(.cbind.regts(list(...), ts_names, suffixes, union = union))
}


# Returns the names of the ... arguments.
# For example, if cbind was called as cbind(a, b), then this
# function returns c("a", "b"). However, if cbind was called as
# cbind(x = a, y= b), then the returned value if c("x", "y").
# This code is based on the function stats:::.makeNamesTs.
.get_ts_names <- function(...) {

  l <- as.list(substitute(list(...)))[-1L]
  nm <- names(l)

  fixup <- if (is.null(nm)) {
    seq_along(l)
  } else {
    nm == ""
  }

  if (!is.null(nm) && !any(fixup)) {
    return(nm)
  }

  default_names <- paste0("x_", seq_along(l))
  create_name <- function(x, default_name) {
    if (is.language(x)) {
      return(deparse(x)[1L])
    } else {
      # this case occurs if cbind is called via do.call(cbind, args)
      return(default_name)
    }
  }
  dep <- mapply(create_name, l[fixup], default_names[fixup])

  if (is.null(nm)) {
    return(dep)
  }
  if (any(fixup)) {
    nm[fixup] <- dep
  }
  return(nm)
}

# This function is based on stats:::cbind.ts.
# sers: objects to be joined
# nmsers: names of the objects in the function call
# suffixes: suffices added to the timeseries in case of duplicate columns
.cbind.regts <- function(sers, nmsers, suffixes, union = TRUE,
                         check_dupl = TRUE) {

  # total number of supplied objects to join
  nser_tot <- length(sers)

  # remove all NULL objects (remember to update suffixes when suffixes is
  # needed below).
  null_objects <- vapply(sers, is.null, NA)
  sers <- sers[!null_objects]
  nmsers <- nmsers[!null_objects]

  nser <- length(sers)
  if (nser == 0L) return(NULL)

  # check if there is any timeseries in sers
  tsser <- vapply(sers, function(x) inherits(x, "ts"), NA)
  if (!any(tsser)) stop("no time series supplied")

  # convert all timeseries to a a regts
  sers[tsser] <- lapply(sers[tsser], as.regts)

  #
  # determine period range result from all timeseries, including timeseries
  # with zero columns.
  #
  ranges <- lapply(sers[tsser], get_period_range)
  freqs <- vapply(ranges, frequency, NA_real_)
  if (length(unique(freqs)) > 1) {
    stop("not all series have the same frequency")
  }
  if (length(unique(ranges)) > 1) {
    if (union) {
      range <- Reduce(range_union, ranges)
    } else {
      range <- Reduce(range_intersect, ranges)
    }
    sers[tsser] <- lapply(sers[tsser], FUN = function(x) {x[range]})
  } else {
    range <- ranges[[1]]
  }
  nperiods <- nperiod(range)

  # get number of columns for the objects to be joined
  nsers <- vapply(sers, FUN = NCOL,  NA_integer_)

  #
  # now remove timeseries with zero columns.
  #
  zero_cols  <- nsers == 0
  sers <- sers[!zero_cols]
  nmsers <- nmsers[!zero_cols]
  tsser <- tsser[!zero_cols]
  nsers <- nsers[!zero_cols]
  nser <- length(sers)

  if (nser == 0) {
    # all objects to join have zero columns
    return(regts(matrix(NA_real_, ncol = 0, nrow = nperiods), period = range))
  }

  # convert non-timeseries objects to timeseries witht the correct range.
  if (any(!tsser)) {
    ln <- vapply(sers[!tsser], NROW, 1)
    if (any(ln != 1 & ln != nperiods)) {
      stop("non-time series not of the correct length")
    }
    sers[!tsser] <- lapply(sers[!tsser],
                           function(x) {regts(x, period = range)})
  }

  # get list of column names of the objects ot be joined
  cnames <- lapply(sers, FUN = colnames)

  #
  # fix missing column names
  #
  no_cnames <- vapply(cnames, FUN = is.null, NA)
  if (any(no_cnames)) {
    # create colnames for arguments without colnames.
    # this mechanism is the same as in ts.intersect and ts.union.
    create_colnames <- function(x, nc, xname) {
      if (nc > 1) {
        return(paste(xname, 1 : nc, sep = "."))
      } else {
        return(xname)
      }
    }
    cnames[no_cnames] <- mapply(create_colnames, sers[no_cnames],
                                nsers[no_cnames], nmsers[no_cnames],
                                SIMPLIFY = FALSE)
  }

  #
  # fix duplicate names
  #
  if (nser > 1 && check_dupl) {
    # check for duplicate names in different timeseries objects, but ignore
    # duplicates in the same timeseries objects
    cnames_unique <- lapply(cnames, FUN = unique)
    all_names <- unlist(cnames_unique)
    if (anyDuplicated(all_names)) {
      dupl_names <- unique(all_names[duplicated(all_names)])
      if (missing(suffixes)) {
        stop(paste0("Duplicate column names (", paste(dupl_names, collapse = " "),
                    "). Specify argument suffixes."))
      } else if (length(suffixes) < nser_tot) {
        stop(paste0("Length of argument 'suffixes' is smaller than the",
                    " number of objects to be joined (", nser_tot,
                    ")."))
      }
      suffixes <- suffixes[!null_objects][!zero_cols]
      add_suffix <- lapply(cnames, function(x) x %in% dupl_names)
      fix_names <- vapply(add_suffix, FUN = any, NA)
      fix_name <- function(cnames, add_suff, suff) {
        return(ifelse(add_suff, paste0(cnames, suff), cnames))
      }
      cnames[fix_names] <- mapply(fix_name, cnames[fix_names],
                                  add_suffix[fix_names], suffixes[fix_names],
                                  SIMPLIFY = FALSE)
    }
  }

  #
  # bind columns together
  #
  last_cols <- cumsum(nsers)
  first_cols <- c(1, last_cols + 1)
  mat_data <- matrix(NA, nrow = nperiods, ncol = last_cols[nser])
  for (i in 1 : nser) {
    mat_data[  , (first_cols[i] : last_cols[i])] <- sers[[i]]
  }

  # create regts object
  names <- unname(unlist(cnames, use.names = FALSE))
  ret <- regts(mat_data, period = range, names = names)

  # handle labels
  if (!is.null(lbls <- .create_labels(sers))) ts_labels(ret) <- lbls

  return(ret)
}


.create_labels <- function(sers) {
  # Add labels from a list of timeseries objects in args to x.

  if (all(sapply(sers, FUN = function(x) is.null(ts_labels(x))))) {
    # no arguments with labels
    return(NULL)
  }

  get_labels <- function(x) {
    # Returns the ts_labels of an R object, or a vector of empty
    # labels if x does not have labels
    if (is.null(x)) {
      return(NULL)
    }
    lbls <- ts_labels(x)
    if (!is.null(lbls)) {
      return(lbls)
    } else {
      return (rep("", NCOL(x)))
    }
  }

  return(do.call(c, unname(lapply(sers, FUN = get_labels))))
}
