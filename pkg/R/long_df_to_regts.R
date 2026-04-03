#' Converts a long data frame to a regts object.
#'
#' The data frame should have at least three columns: one column with
#' the names of the timeseries, one column with periods and a third
#' column with the values. The names of these columns can be specified with
#' argument `name_col`, `period_col` and `value_col`, respectively
#' (the default column names are `"name"`, `"period"` and `"value"`).
#' \cr
#' Argument `label_col` can be used to specify the name of a column with
#' labels (default `"label"`). If not specified, we function checks whether
#' a column `"label"` exists. If this column exists, the labels are based on
#' the data in this column. If the column does not exist, the label column
#' is ignored. If the data frame does contain a column `"label"` but you do not
#' want to create labels specify `label_col = NULL`.
#'
#' @param df A long data frame (or \code{\link[tibble]{tibble}}) with at least
#' three columns with names specified with arguments `name_col`, `period_col`
#' and `value_col`.
#' @param name_col The name of the column with variable names (by default
#' `"name"`).
#' @param period_col The name of the column with periods. This columns
#' should contain data of a type that can be converted to `period` objects
#' by function \code{\link{period}}. The default is `"period"`.
#' @param value_col The name of the column with values (default `"value"`).
#' @param label_col The name of the column with labels (default `"label`").
#' If not specified and a column named `"label"` exists, the texts in this
#' column are used to create timeseries labels. Specify `NULL` if you do not
#  want to use the labels in column `labels`.
#' @param numeric A logical. If `TRUE` (the default), the data in the column
#' with values are converted to numeric data.
#'
#' @returns A \code{regts} object
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#' ~name,  ~period,  ~value,  ~description,
#' "a",    "2015Q3", 1.2 ,     "Var a",
#' "a",    "2016Q1", 1.5 ,     "Var a",
#' "b",    "2015Q3", 15 ,      "Var b",
#' "b",    "2015Q4", 20,       "Var b"
#'  )
#' long_df_to_regts(df, label_col = "description")
long_df_to_regts <- function(df, name_col = "name",
                             period_col = "period",
                             value_col = "value",
                             label_col = "label",
                             numeric = TRUE) {

  df <- as.data.frame(df)

  cnames <- colnames(df)

  # Check if the specified column names are distinct
  data_cols <- c(name_col, period_col, value_col)
  all_cols <- data_cols
  if (!is.null(label_col)) all_cols <- c(all_cols, label_col)
  if (anyDuplicated(all_cols)) {
    cols_dupl <- unique(all_cols[duplicated(all_cols)])
    stop("name_col, period_col, value_col and label_col should be distinct.\n",
         "Duplicate column names: ", paste(cols_dupl, collapse = ", "))
  }

  # Check if data columns exist
  missing_cols <- setdiff(data_cols, cnames)
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist: ",
         paste(missing_cols, collapse = ", "))
  }

  # Now check the label column
  if (missing(label_col)) {
    # if label_col not specified and the default label column name does not
    # exist, then set label_col to NULL
    if (!label_col %in% cnames) label_col <- NULL
  } else if (!is.null(label_col) && !label_col %in% cnames) {
    stop("Label column (", label_col, ") does not exist.")
  }

  df_data <- select(df, all_of(name_col), all_of(period_col),
                    all_of(value_col))

  # Check that each name-period combination is unique.
  df_names_periods <- select(df_data, all_of(name_col), all_of(period_col))
  dupl <- df_names_periods[duplicated(df_names_periods), , drop = FALSE]
  if (nrow(dupl) > 0) {
    stop("Duplicate rows found:\n",
         paste0("  - name: ", dupl[[name_col]], ", period: ",
                dupl[[period_col]], collapse = "\n"))
  }

  # Now convert to wide format
  df_data <- df_data |>
    pivot_wider(names_from = all_of(name_col), values_from = all_of(value_col))

  ts_data <- as.regts.data.frame(df_data, time_column = period_col,
                                 numeric = numeric)

  if (!is.null(label_col)) {
    labels <- get_labels_from_long_df(df, name_col, label_col)
    ts_data <- update_ts_labels(ts_data, labels)
  }

  return(ts_data)
}

#' Create a named character vector with labels from a long data frame.
#'
#' If there are duplicate labels for a name, the first label is used.
#' A warning is given for all duplicate labels.
#' @param df A long data frame.
#' @param name_col Character: name of the column with names.
#' @param label_col Character: name of the column with labels.
#'
#' @returns A named character vector with labels.
#' @noRd
get_labels_from_long_df <- function(df, name_col, label_col) {

  df <- select(df, all_of(name_col), all_of(label_col)) |>
    dplyr::distinct()

  # Find names with duplicate labels
  duplicates <- df |>
    dplyr::group_by(.data$name) |>
    dplyr::filter(dplyr::n_distinct(.data[[label_col]]) > 1) |>
    dplyr::summarise(labels = paste(paste0("'", .data[[label_col]], "'"),
                                    collapse = ", "),
                     .groups = "drop")
  if (nrow(duplicates) > 0) {
    warning_msg <- paste0(
      "Duplicate labels found for the following names (the first label is ",
      "used):\n",
      paste0("  - ", duplicates$name, ": ",
             paste(duplicates$labels, collapse = "\n"))
    )
    warning(warning_msg, call. = FALSE)
  }

  # Keep the first row for each name
  df_unique <- df[!duplicated(df[[name_col]]), ]

  # Create a named vector
  setNames(df_unique[[label_col]], df_unique[[name_col]])
}
