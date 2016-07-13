#' Join two or more timeseries
#'
#' Bind time series which have a common frequency.
#' @param  ... two or more univariate or multivariate time series,
#' or objects which can coerced to time series
#' @param method method used to determine the time axis of the result.
#' Specify \code{"union"} for a total time coverage
#' (missing values are filled with \code{NA}) and \code{"intersect"}
#' to restrict the time axis to the time covered by all timeseries.
#' @param suffixes Suffixes appended to the column names for overlapping
#' columns. This argument is mandatory if the timeseries have overlapping
#' columns.
#' @export
join_ts <- function(..., union = TRUE, suffixes) {
    if (union) {
        ret <- ts.union(...)
    } else {
        ret <- ts.intersect(...)
    }

    # the following code is used to create unique column names for the
    # return value. This code is inspired by the merge.zoo function of
    # package zoo

    args <- list(...)
    cnames <- lapply(args, FUN = colnames)

    # handle missing columns
    missing_colnames <- lapply(cnames, FUN = is.null)
    if (any(unlist(missing_colnames))) {
        argnames <- sapply(as.list(substitute(list(...)))[-1L], deparse)
        get_colnames <- function(i) {
            if (missing_colnames[[i]]) {
                nc <- ncol(args[[i]])
                if (!is.null(nc) && nc > 1) {
                    ret <- paste(argnames[[i]], 1 : nc, sep = "_")
                } else {
                    ret <- argnames[[i]]
                }
            } else {
                ret <- cnames[[i]]
            }
            return (ret)
        }
        cnames <- lapply(seq_along(args), FUN = get_colnames)
    }

    cnames_unique <- lapply(cnames, FUN = unique)
    all_names <- unlist(cnames_unique)
    dupl <- duplicated(all_names)

    if (any(dupl)) {
        #TODO: improve error messages
        if (missing(suffixes)) {
            stop("Duplicate column names. Specify argument suffixes ")
        } else if (length(suffixes) < length(args)) {
            stop("Length of argument suffixes is incorrect")
        }
        add_suffix <- lapply(cnames, function(x) x %in% all_names[dupl])
        fix_dupl <- function(i) {
            return (ifelse(add_suffix[[i]], paste0(cnames[[i]], suffixes[i]),
                                                   cnames[[i]]))
        }
        cnames  <- lapply(seq_along(args), fix_dupl)
    }

    cnames <- unlist(cnames)
    colnames(ret) <- cnames

    ret <- handle_labels(as.regts(ret), ...)

    return (ret)
}

#' Join two or more timeseries
#'
#' Bind time series which have a common frequency.
#' @param  ... two or more univariate or multivariate time series,
#' or objects which can coerced to time series
#' @param method method used to determine the time axis of the result.
#' Specify \code{"union"} for a total time coverage
#' (missing values are filled with \code{NA}) and \code{"intersect"}
#' to restrict the time axis to the time covered by all timeseries.
#' @param suffixes Suffixes appended to the column names for overlapping
#' columns. This argument is mandatory if the timeseries have overlapping
#' columns.
#' @export
join_ts2 <- function(..., union = TRUE, suffixes) {

    ts_names <- stats:::.makeNamesTs(...)
    ret <- stats:::.cbind.ts(list(...), ts_names, dframe = FALSE,
                             union = union)

    # next step: we do not like the column names created by .cbind.ts,
    # therefore determine our own column names
    args <- list(...)
    cnames <- lapply(args, FUN = colnames)

    # handle missing columns
    missing_colnames <- lapply(cnames, FUN = is.null)
    if (any(unlist(missing_colnames)) || TRUE) {
        get_colnames <- function(i) {
            if (missing_colnames[[i]]) {
                nc <- ncol(args[[i]])
                if (!is.null(nc) && nc > 1) {
                    ret <- paste(ts_names[[i]], 1 : nc, sep = "_")
                } else {
                    ret <- ts_names[[i]]
                }
            } else {
                ret <- cnames[[i]]
            }
            return (ret)
        }
        cnames <- lapply(seq_along(args), FUN = get_colnames)
    }

    cnames_unique <- lapply(cnames, FUN = unique)
    all_names <- unlist(cnames_unique)
    dupl <- duplicated(all_names)

    if (any(dupl)) {
        #TODO: improve error messages
        if (missing(suffixes)) {
            stop("Duplicate column names. Specify argument suffixes ")
        } else if (length(suffixes) < length(args)) {
            stop("Length of argument suffixes is incorrect")
        }
        add_suffix <- lapply(cnames, function(x) x %in% all_names[dupl])
        fix_dupl <- function(i) {
            return (ifelse(add_suffix[[i]], paste0(cnames[[i]], suffixes[i]),
                           cnames[[i]]))
        }
        cnames  <- lapply(seq_along(args), fix_dupl)
    }

    cnames <- unlist(cnames)
    colnames(ret) <- cnames

    ret <- handle_labels(as.regts(ret), ...)

    return (ret)
}
