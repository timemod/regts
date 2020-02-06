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

  args <- list(...)

  # ts.union and ts.intersect do not handle multivariate ts objects with
  # zero columns properly, therefore replace them with NULL
  args <- lapply(args, FUN = function(x) {
                   if (NCOL(x) == 0) return(NULL) else return(x)})

  ts_names <- .get_ts_names(...)

  ret <- .cbind.regts(args, ts_names, union = union)

  if (!is.mts(ret)) {
    return(ret)
  }

  cnames <- lapply(args, FUN = colnames)

  # Creates column names for arguments that do not have column names.
  # We do not like the column names created by ts.union and ts.intersect.
  missing_colnames <- sapply(cnames, FUN = is.null)
  if (any(missing_colnames)) {
    # create colnames for arguments without colnames.
    # this mechanism is the same as in ts.intersect and ts.union.

    get_colnames <- function(i) {
      if (missing_colnames[[i]]) {
        if (is.null(args[[i]])) {
          return(NULL)
        }
        nc <- NCOL(args[[i]])
        if (nc == 0) {
          return(NULL)
        } else if (nc > 1) {
          return(paste(ts_names[[i]], 1 : nc, sep = "."))
        } else {
          return(ts_names[[i]])
        }
      } else {
        return(cnames[[i]])
      }
    }
    cnames <- lapply(seq_along(args), FUN = get_colnames)
  } else {
    cnames <- unname(cnames)
  }

  #
  # handle non-unique names
  #
  cnames_unique <- lapply(cnames, FUN = unique)
  all_names <- unlist(cnames_unique)

  if (anyDuplicated(all_names)) {
    dupl <- duplicated(all_names)
    if (missing(suffixes)) {
      stop(paste0("Duplicate column names (",
                  paste(unique(all_names[dupl]), collapse = " "),
                  "). Specify argument suffixes."))
    } else if (length(suffixes) < length(args)) {
      stop(paste0("Length of argument suffixes is smaller than the",
                  " number of objects to be joined (", length(args),
                  ")."))
    }
    add_suffix <- lapply(cnames, function(x) x %in% all_names[dupl])
    fix_dupl <- function(i) {
      return (ifelse(add_suffix[[i]], paste0(cnames[[i]], suffixes[i]),
                     cnames[[i]]))
    }
    cnames <- lapply(seq_along(args), fix_dupl)
  }

  cnames <- unlist(cnames)
  colnames(ret) <- cnames

  return(add_labels(as.regts(ret), args))
}

.get_ts_names <- function(...) {
  # Returns the names of the ... arguments.
  # For example, if cbind was called as cbind(a, b), then this
  # function returns c("a", "b"). However, if cbind was called as
  # cbind(x = a, y= b), then the returned value if c("x", "y").
  # This code is based on the function stats:::.makeNamesTs.

  l <- as.list(substitute(list(...)))[-1L]
  nm <- names(l)
  fixup <- if (is.null(nm)) {
    seq_along(l)
  } else {
    nm == ""
  }
  dep <- sapply(l[fixup], function(x) deparse(x)[1L])
  if (is.null(nm)) {
    return(dep)
  }
  if (any(fixup)) {
    nm[fixup] <- dep
  }
  return (nm)
}

#' @importFrom stats tsp
# This function is a copy of stats:::cbind.ts
.cbind.regts <- function (sers, nmsers, union = TRUE)
{
  nulls <- vapply(sers, is.null, NA)
  sers <- sers[!nulls]
  nser <- length(sers)
  if (nser == 0L)
    return(NULL)
  if (nser == 1L) return(sers[[1L]])
  tsser <- vapply(sers, function(x) length(tsp(x)) > 0L, NA)
  if (!any(tsser))
    stop("no time series supplied")
  sers <- lapply(sers, as.ts)
  nsers <- vapply(sers, NCOL, 1)
  tsps <- sapply(sers[tsser], tsp)
  freq <- mean(tsps[3, ])
  if (max(abs(tsps[3, ] - freq)) > getOption("ts.eps")) {
    stop("not all series have the same frequency")
  }
  if (union) {
    st <- min(tsps[1, ])
    en <- max(tsps[2, ])
  }
  else {
    st <- max(tsps[1, ])
    en <- min(tsps[2, ])
    if (st > en) {
      warning("non-intersecting series")
      return(NULL)
    }
  }
  p <- c(st, en, freq)
  n <- round(freq * (en - st) + 1)
  if (any(!tsser)) {
    ln <- vapply(sers[!tsser], NROW, 1)
    #if (any(ln != 1 && ln != n))
    if (any(ln != 1 & ln != n))
      stop("non-time series not of the correct length")
    for (i in (1L:nser)[!tsser]) {
      sers[[i]] <- ts(sers[[i]], start = st, end = en,
                      frequency = freq)
    }
    tsps <- sapply(sers, tsp)
  }

  ns <- sum(nsers)
  x <- matrix(NA, n, ns)
  cs <- c(0, cumsum(nsers))
  nm <- character(ns)
  for (i in 1L:nser) if (nsers[i] > 1) {
    cn <- colnames(sers[[i]])
    if (is.null(cn))
      cn <- 1L:nsers[i]
    nm[(1 + cs[i]):cs[i + 1]] <- paste(nmsers[i], cn,
                                       sep = ".")
  }
  else nm[cs[i + 1]] <- nmsers[i]
  dimnames(x) <- list(NULL, nm)

  for (i in 1L:nser) {
    if (union) {
      xx <- if (nsers[i] > 1)
        rbind(matrix(NA, round(freq * (tsps[1, i] - st)),
                     nsers[i]), sers[[i]], matrix(NA, round(freq *
                                                              (en - tsps[2, i])), nsers[i]))
      else c(rep.int(NA, round(freq * (tsps[1, i] - st))),
             sers[[i]], rep.int(NA, round(freq * (en - tsps[2,
                                                            i]))))
    }
    else {
      xx <- window(sers[[i]], st, en)
    }
    x[, (1 + cs[i]):cs[i + 1]] <- xx
  }

  return(ts(x, start = st, frequency = freq))
}


add_labels <- function(x, args) {
  # Add ts_labels in args to x.

  if (all(sapply(args, FUN = function(x) is.null(ts_labels(x))))) {
    # no arguments with labels
    return(x)
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

  labels <- unname(unlist(lapply(args, FUN = get_labels)))
  ts_labels(x) <- labels
  return(x)
}



