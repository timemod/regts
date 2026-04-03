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
