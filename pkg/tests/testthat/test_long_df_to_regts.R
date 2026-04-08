library(regts)
library(testthat)
library(dplyr)

rm(list = ls())

df <- tibble::tribble(
  ~name,  ~period,  ~value,  ~description,
  "a",    "2015Q3", 1.2,     "Var a",
  "a",    "2016Q1", 1.5,     "Var a",
  "b",    "2015Q3", 15,      "Var b",
  "b",    "2015Q4", 20,       "Var b",
  "b",    "2016Q1", NA_real_, "Var b (2)"
)

ts_expected <- cbind(
  a = regts(c(1.2, NA, 1.5), start = "2015Q3"),
  b = regts(c(15, 20, NA), start = "2015Q3")
)
ts_expected_lbls <- update_ts_labels(ts_expected,
                                     c(a = "Var a", b = "Var b"))

wmsg_dupl_labels <- paste(
  "Duplicate labels found for the following names (the first label is used):\n",
  " - b: 'Var b', 'Var b (2)'"
)

test_that("basic example", {

  expect_equal(
    long_df_to_regts(df),
    ts_expected
  )

  expect_warning(
    expect_equal(
      long_df_to_regts(df, label_col = "description"),
      ts_expected_lbls
    ),
    wmsg_dupl_labels,
    fixed = TRUE
  )
})

test_that("column 'label' in df", {
  df_tmp <- df |>
    rename(label = "description")

  expect_warning(
    expect_equal(
      long_df_to_regts(as.data.frame(df_tmp)),
      ts_expected_lbls
    ),
    wmsg_dupl_labels,
    fixed = TRUE
  )

  expect_equal(
    long_df_to_regts(df_tmp, label_col = NULL),
    ts_expected
  )
})

test_that("no observations", {
  df_tmp <- filter(df, .data$name == "xxx")
  expect_error(
    long_df_to_regts(df_tmp),
    "'regts' object must have one or more observations",
    fixed = TRUE
  )
})

test_that("column name errors", {

  df_tmp <- df[, 2]
  expect_error(
    long_df_to_regts(df_tmp),
    "The following columns do not exist: name, value",
    fixed = TRUE
  )

  msg <- paste0(
    "name_col, period_col, value_col and label_col should be distinct.\n",
    "Duplicate column names: name"
  )
  expect_error(
    long_df_to_regts(df, label_col = "name"),
    msg,
    fixed = TRUE
  )

  msg <- paste0(
    "name_col, period_col, value_col and label_col should be distinct.\n",
    "Duplicate column names: xxx"
  )
  expect_error(
    long_df_to_regts(df, label_col = NULL, name_col = "xxx",
                     period_col = "xxx", value_col = "xxx"),
    msg,
    fixed = TRUE
  )

  expect_error(
    long_df_to_regts(df, label_col = NULL, name_col = "xxx",
                     period_col = "yyy"),
    "The following columns do not exist: xxx, yyy"
  )
})


test_that("duplicate rows", {
  df_tmp <- df
  df_tmp$period <- "2015q1"
  msg <- paste0("Duplicate rows found:\n",
                "  - name: a, period: 2015q1\n",
                "  - name: b, period: 2015q1")
  expect_error(
    long_df_to_regts(df_tmp),
    msg,
    fixed = TRUE
  )
})

test_that("illegal period", {
  df_tmp <- df
  df_tmp$period <- paste0("X", df_tmp$period)
  expect_error(
    long_df_to_regts(df_tmp),
    "Illegal period X2015Q3.",
    fixed = TRUE
  )
})

test_that("character values", {

  df_tmp <- df
  df_tmp$value <- as.character(df$value)
  expect_equal(
    long_df_to_regts(df_tmp),
    ts_expected
  )

  ts_expected_txt <- ts_expected
  ts_expected_txt[] <- as.character(ts_expected_txt)
  expect_equal(
    long_df_to_regts(df_tmp, numeric = FALSE),
    ts_expected_txt
  )


  df_tmp$value[5] <- "hello"
  wmsg <- paste0(
    "The following texts could not be converted to numeric:\n",
    "\"hello\""
  )
  expect_warning(
    expect_equal(
      long_df_to_regts(df_tmp),
      ts_expected
    ),
    wmsg,
    fixed = TRUE
  )

  ts_expected_txt2 <- ts_expected_txt
  ts_expected_txt2["2016Q1", "b"] <- "hello"
  expect_warning(
    expect_equal(
      long_df_to_regts(df_tmp, numeric = FALSE),
      ts_expected_txt2
    ),
    NA
  )

})

test_that("NA values at beginning and end periods", {
  df_tmp <- tibble::tribble(
    ~name,  ~period,  ~value,   ~description,
    "a",    "2015Q3", NA_real_,     "Var a",
    "a",    "2016Q1", 1.5,           "Var a",
    "b",    "2015Q4", 20,            "Var b",
    "b",    "2016Q2", NA_real_,      "Var b"
  )

  expect_equal(
    get_period_range(long_df_to_regts(df_tmp)),
    period_range("2015Q3/2016Q2")
  )
})
