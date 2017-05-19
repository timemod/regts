#' Write timeseries to a csv file
#'
#' This function writes timeseries to a csv file.
#' The csv file is actually written by function \code{\link[data.table]{fwrite}}
#' of package \code{data.table}.
#' @param x a \code{ts} or \code{regts} object
#' @param file a \code{regts} object
#' @param rowwise a logical value: should the timeseries we written rowwise?
#' @param sep The separator between columns. Default is ",".
#' @param dec The decimal separator, by default ".". Cannot be the same as sep.
#' @param labels should labels we written, and if so before the names or after
#' the names? By default, labels are written after the names if present.
#' @importFrom data.table fwrite
#' @export
write_ts_csv <- function(x, file, rowwise = TRUE, sep = ",", dec = ".",
                        labels = c("after", "before", "no")) {

  labels_missing <- missing(labels)

  if (!labels_missing) {
    labels <- match.arg(labels)
  }

  df_info <- write_ts_df(x, rowwise, labels, labels_missing)

  fwrite(df_info$df, file, sep = sep, dec  = dec)

  return(invisible(NULL))
}

#' Write timeseries to an xlsx file
#'
#' This function writes timeseries to an xlsx  file.
#' The xlsx file is actually written by function \code{\link[xlsx]{write.xlsx}}
#' of package \code{data.table}.
#' @param x a \code{ts} or \code{regts} object
#' @param file a \code{regts} object
#' @param sheet_name the sheet name
#' @param append_sheet If \code{FALSE} (the default), then the original file,
#' if it exists, is replaced with the new file. All original data is lost.
#' If \code{TRUE}, then only data on the sheet with
#' the specified sheetname is erased and replaced with new data.  If the sheet
#' does not yet exist, then a new sheet is created and appended to the
#' original file.
#' @param rowwise a logical value: should the timeseries we written rowwise?
#' @param labels should labels we written, and if so before the names or after
#' the names? By default, labels are written after the names if present
#' @importFrom xlsx loadWorkbook
#' @importFrom xlsx getSheets
#' @importFrom xlsx removeRow
#' @importFrom xlsx createSheet
#' @importFrom xlsx createWorkbook
#' @importFrom xlsx CellStyle
#' @importFrom xlsx Alignment
#' @importFrom xlsx addDataFrame
#' @importFrom xlsx saveWorkbook
#' @importFrom xlsx createFreezePane
#' @importFrom xlsx autoSizeColumn
#' @importFrom xlsx removeRow
#'
#'
#' @export
write_ts_xlsx <- function(x, file, sheet_name = "Sheet1",
                          append_sheet = FALSE,
                          rowwise = TRUE,
                          labels = c("after", "before", "no")) {

  if (!file.exists(file)) {
    append_sheet <- FALSE
  }

  if (append_sheet) {
    wb <- loadWorkbook(file)
    sheets <- getSheets(wb)
    if (sheet_name %in% names(sheets)) {
      sheet <- sheets[[sheet_name]]
      removeRow(sheet)
    } else {
      sheet  <- createSheet(wb, sheetName = sheet_name)
    }
  } else {
    if (file.exists(file)) {
      unlink(file)
    }
    wb <- createWorkbook()
    sheet  <- createSheet(wb, sheetName = sheet_name)
  }

  labels_missing <- missing(labels)

  if (!labels_missing) {
    labels <- match.arg(labels)
  }

  df_info <- write_ts_df(x, rowwise, labels, labels_missing)


  if (rowwise) {
    n_text_rows <- 1
    n_text_cols <- 1 + as.integer(df_info$has_labels)
    row_split <- 2
    col_split <- 1 + n_text_cols
  } else {
    n_text_rows <- 1 + as.integer(df_info$has_labels)
    n_text_cols <- 1
    row_split <- 2 + as.integer(df_info$has_labels)
    col_split <- 2
  }

  right_align_style <- CellStyle(wb, alignment = Alignment(horizontal =
                                                             "ALIGN_RIGHT"))

  # write the column headers, use right alignment for the numeric columns
  # for columnwise timeseries with labels, also write the label row
  column_headers <- as.data.frame(t(colnames(df_info$df)),
                                  stringsAsFactors = FALSE)
  if (!rowwise & df_info$has_labels) {
    lbls <- as.character(df_info$df[1, , drop = FALSE])
    column_headers <- rbind(column_headers, lbls, stringsAsFactors = FALSE)
    df_info$df <- df_info$df[-1, , drop = FALSE]
  }
  col_style <- rep(list(right_align_style), ncol(column_headers) - n_text_cols)
  names(col_style) <- seq(n_text_cols + 1, ncol(column_headers))
  addDataFrame(column_headers, sheet, col.names = FALSE, row.names = FALSE,
               colStyle = col_style)

  # now write the data part

  # TODO: number format
  # nf <- CellStyle(wb, dataFormat=DataFormat("0.000"))
  # dfColIndex <- rep(list(nf), dim(zoo1)[2])
  # names(dfColIndex) <- seq(1, dim(zoo1)[2], by = 1)
  col_style <- NULL

  addDataFrame(df_info$df, sheet, col.names = FALSE, row.names = FALSE,
               startRow = n_text_rows + 1, colStyle = col_style)

  autoSizeColumn(sheet, seq(1, dim(df_info$df)[2]))

  createFreezePane(sheet, rowSplit = row_split, colSplit = col_split)

  saveWorkbook(wb, file)

  return(invisible(NULL))
}


# write timeseries to a data frame that can be written to a csv or excel file
write_ts_df <- function(x, rowwise, labels, labels_missing) {

  if (!is.ts(x)) {
    stop(paste("Argument x is not a timeseries object but a ", class(x)))
  }

  if (labels_missing || labels != "no") {
    lbls <- ts_labels(x)
    if (labels_missing) {
      if (is.null(lbls)) {
        labels <- "no"
      } else {
        labels <- "after"
      }
    } else {
      if (is.null(lbls)) {
        lbls <- rep("", NCOL(x))
      }
    }
  }

  # remove the labels, we don't need them any more
  ts_labels(x) <- NULL

  df <- as.data.frame(x)

  if (rowwise) {
    df <- transpose_df(df)
    names <- rownames(df)
    if (labels == "no") {
      df <- cbind(name = names, df, stringsAsFactors = FALSE)
    } else if (labels == "after") {
      df <- cbind(name = names, label = lbls, df, stringsAsFactors = FALSE)
    } else if (labels == "before") {
      df <- cbind(label = lbls, name = names, df,stringsAsFactors = FALSE)
    }
  } else {
    df <- cbind(period = rownames(df), df, stringsAsFactors = FALSE)
    if (labels == "after") {
      df <- rbind(c("label", lbls), df, stringsAsFactors = FALSE)
    } else if (labels == "before") {
      stop("For columnwise timeseries labels option \"before\" is not allowed")
    }
  }

  rownames(df) <- NULL
  return(list(df = df,  has_labels = labels != "no"))
}


