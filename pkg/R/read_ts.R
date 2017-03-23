#' Reads timeseries from a data frame
#'
#' @param df a \code{data.frame}
#' @param columnwise a logical value: are the timeseries stored columnwise?
#' If not specified, then \code{read_ts} tries to figure out itself if
#' the timeseries are stored columnwise or rowwise
#' @param labels label option: should labels be read or not. See details.
#' @return a \code{regts} object
#' @export
read_ts <- function(df, columnwise, labels = c("no", "after", "before")) {

    labels <- match.arg(labels)

    # TODO: what about stringAsFactors?

    if (missing(columnwise)) {
        columnwise <- !any(is_period_text(colnames(df)))
    }

    if (columnwise) {
        return(read_ts_columnwise(df, labels))
    } else {
        return(read_ts_rowwise(df, labels))
    }
}

# returns true if x is a positive integer
is_posint <- function(x) {
    return(grepl("^\\d+$", as.character(x)))
}

# Standard columnwise format:
#  - variable names in column names
#  - optionally labels in first row (TO DO!)
#  - time axis in rownames, or any other column.
#    columns before the time column are skipped
read_ts_columnwise <- function(df, labels) {

    if (labels == "before") {
        stop(paste("For columnwise timeseries, the label option 'before'",
                    "is not allowed"))
    }

    period_info <- find_period_column(df)
    col_nr <- period_info$col_nr
    is_period <- period_info$is_period

    # Ff colr_nr > 1, then remove all previous columns
    # NOTE: we have to do this before rows without period are removed,
    # because the latter procedure will also remove empty columns
    if (col_nr > 1) {
        df <- df[, -(1:col_nr-1), drop = FALSE]
        col_nr <- 1
    }

    # remove rows without period
    df <- df[is_period, , drop = FALSE]

    return(as.regts(df, time_column = col_nr))
}

# Find a period column for a standard (columnwise) data frame.
# Returns a vector which two elements:
#  * the first element is the period column: 0 if the period is in the row names,
#     >= 1 if in a column of the data frame and -1 if not found
#  * the second element is a logical vector: TRUE if the correponding
#    element of the period column is a period
find_period_column <- function(df) {

    # The standard row names of a data frame are row numbers.
    # Unfortunately, we cannot distinguish between standard row names
    # or year indicators (if we know the frequency we can do more)
    standard_rownames <- all(is_posint(rownames(df)))

    if (!standard_rownames) {
        # check if the row names contain periods
        is_period <- is_period_text(rownames(df))
        if (any(is_period)) {
            row_nr <- Position(function(x) {x}, is_period)
            return(list(col_nr = 0, is_period = is_period))
        }
    }

    # try to find the first column with period.
    for (i in 1:ncol(df)) {
        is_period <- is_period_text(df[, i])
        if (any(is_period)) {
            col_index <- i
            row_nr <- Position(function(x) {x}, is_period)
            return(list(col_nr = i, is_period = is_period))
        }
    }

    # We have not found a time column yet. If the row names are
    # integers, then we assume that they are years
    # TODO: do not do this if frequency > 1 has been specified)
    if (standard_rownames) {
        return(list(col_nr = 0, is_period = rep(TRUE, nrow(df))))
    }
}

# Rowwise format:
#  - time indicators in colnames
#  - variable names in rownames or first column,
#    optionally labels in the second column
#  - optionally labels in first column, variable names second column
#    (argument labels_first)
read_ts_rowwise <- function(df, labels) {

    is_period <- is_period_text(colnames(df))

    first_data_col <- Position(function(x) {x}, is_period)

    standard_rownames <- all(is_posint(rownames(df)))

    if (standard_rownames) {

        # the row names do not contain variable names
        if (labels == "before") {
            colname_column <- first_data_col - 1
            label_columns <- seq_len(colname_column - 1)
        } else {
            colname_column <- 1
            if (first_data_col > colname_column + 1) {
                label_columns <- (colname_column + 1) : (first_data_col - 1)
            } else {
                label_columns <- integer(0)
            }
        }

        # remove rows without variable names
        df <- df[nchar(df[, colname_column]) > 0, , drop = FALSE]

    } else {

        colname_column <- 0

        # remove empty row names
        if (colname_column == 0) {
            df <- df[nchar(rownames(df)) > 0, , drop = FALSE]
        }

        # variable names in the row names
        if (labels == "before") {
            stop(paste("Label option 'before' is not allowed",
                       "if the row names are not numbered"))
        }

        label_columns <- seq_len(first_data_col - 1)
    }

    if (labels != "no") {
        labels <- df[, label_columns, drop = FALSE]
        l <- lapply(labels, as.character)
        labels <- do.call(paste, l)
        labels <- trimws(labels)
    }

    # remove all columns without period except for the colname column
    keep_cols <- is_period
    if (colname_column >= 1) {
        keep_cols[colname_column] <- TRUE
    }
    df <- df[ ,  keep_cols, drop = FALSE]

    if (colname_column > 1) {
        colname_column <- 1
    }

    if (colname_column >= 1) {
        df <- transpose_df(df, colname_column = colname_column)
    } else {
        df <- transpose_df(df)
    }

    # remove empty column names
    df <- df[ , nchar(colnames(df)) > 0, drop = FALSE]

    ret <- as.regts(df)
    if (labels != "no" && any(nchar(labels) > 0)){
        ts_labels(ret) <- labels
    }
    return(ret)
}
