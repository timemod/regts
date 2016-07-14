#' Join two or more timeseries
#'
#' Join time series objects with a common frequency.
#'
#' @param  ... two or more univariate or multivariate time series,
#' or objects which can coerced to time series
#' @param union if \code{TRUE}, then the period range of the result
#' is the union of the period ranges of the joined objects
#' (missing values are filled with \code{NA}). If \code{FALSE},
#' then the period range of the result is the intersection of the period ranges
#' of the joined objects.
#' @param suffixes Suffixes appended to the column names for overlapping
#' columns. This argument is obligatory if the timeseries have overlapping
#' column names
#' @seealso \code{\link{as.list.regts}}
#' @importFrom stats ts.union ts.intersect
#' @export
join_ts <- function(..., union = TRUE, suffixes) {

    if (union) {
        ret <- ts.union(..., dframe = FALSE)
    } else {
        ret <- ts.intersect(..., dframe = FALSE)
    }

    # The following code is to create column names for the result.
    # We do not like the column names created by ts.union and ts.intersect,
    # therefore determine our own column names
    args <- list(...)
    cnames <- lapply(args, FUN = colnames)

    # handle missing columns
    missing_colnames <- lapply(cnames, FUN = is.null)
    if (any(unlist(missing_colnames))) {
        ts_names <- get_ts_names(...)
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
    } else {
        cnames <- unname(cnames)
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

get_ts_names <- function(...) {
    # copied from stats:::.makeNamesTs
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

# Returns the timeseries label from an arbitrary object.
# Only regts objects have labels
get_labels <- function(x) {
    if (is.ts(x)) {
        lbls <- ts_labels(x)
        if (!is.null(lbls)) {
            return (lbls)
        }
    }
    # no ts_labels present, create empty labels
    nc <- ncol(x)
    if (!is.null(nc)) {
        return (rep("", nc))
    } else {
        return ("")
    }
}

# Check if the arguments in ... contain any label, and if so than add
# all labels to x
handle_labels <- function(x, ...) {
    arguments <- list(...)
    # Check if there are any labels
    has_labels <- any(unlist(lapply(arguments,
                                    FUN = function(x) !is.null(ts_labels(x)))))
    if (has_labels) {
        labels <- unname(unlist(lapply(arguments, FUN = get_labels)))
        ts_labels(x) <- labels
    }
    return(x)
}
