library(regts)
library(testthat)
library(tibble)

rm(list = ls())

context("test internal function inspect_tibble")

get_csv_tbl <- function(tbl) {
  # convert a tbl for xlsx to a tibble for csv
  tbl[] <- lapply(tbl, FUN = as.character)
  return(tbl)
}

wmsg_rw <- paste("Could not determine if timeseries are stored rowwise or",
              "columnwise.\nAssuming rowwise based on the number of periods",
              "found.\nUse argument rowwise if necessary.")
wmsg_cw <- paste("Could not determine if timeseries are stored rowwise or",
                 "columnwise.\nAssuming columnwise based on the number of periods",
                 "found.\nUse argument rowwise if necessary.")


test_that("test1", {

  tbl <- tibble(a = c("2010", "", "a"), b = c("", "2014", "3"))

  expected_result <- list(rowwise = TRUE, period_row = 2L, first_data_col = 2L,
                          last_data_col = 2L, is_data_col = c(FALSE, TRUE),
                          periods = period(2014))

  info1 <- regts:::inspect_tibble(tbl, frequency = NA, xlsx = FALSE)
  expect_identical(info1, expected_result)


  info2 <- regts:::inspect_tibble(tbl, frequency = 1, xlsx = TRUE)

  expect_identical(info2,  list(rowwise = FALSE, period_col = 2L,
                                first_data_row = 2L, last_data_col = NA_integer_,
                                is_data_col = c(FALSE, FALSE),
                                is_data_row = c(FALSE, TRUE, TRUE),
                                names = character(0), lbls = NULL))


  info3 <- regts:::inspect_tibble(tbl, frequency = 1, xlsx = TRUE,
                                  rowwise = TRUE)
  expect_identical(info3, expected_result)


  info4 <- regts:::inspect_tibble(tbl, frequency = NA, rowwise = TRUE,
                                  xlsx = FALSE)
  expect_identical(info4,  expected_result)

  info5 <- regts:::inspect_tibble(tbl, frequency = NA, rowwise = FALSE,
                                  xlsx = TRUE)
  expect_identical(info5, list(rowwise = FALSE, period_col = 2L,
                               first_data_row = 2L, last_data_col = NA_integer_,
                               is_data_col = c(FALSE, FALSE),
                               is_data_row = c(FALSE, TRUE, TRUE),
                               names = character(0), lbls = NULL))

  info6 <- regts:::inspect_tibble(tbl, frequency = 4, xlsx = TRUE)
  expect_null(info6)
})

test_that("test2", {

  tbl1 <- tibble(a = c("2010", "x", "y"), b = c("aap", "z", "g"))
  info1 <- regts:::inspect_tibble(tbl1, frequency = NA, xlsx = TRUE)
  expect_null(info1)

  tbl2 <- tibble(a = list("2010", 1, 2), b = list("aap", 3, 4))
  info2 <- regts:::inspect_tibble(tbl2, frequency = NA, xlsx = TRUE)
  expect_identical(info2, list(rowwise = FALSE, period_col = 1L,
                               first_data_row = 2L, last_data_col = 2L,
                               is_data_col = c(FALSE, TRUE),
                               is_data_row = c(FALSE, TRUE, TRUE),
                               names = "aap", lbls = NULL))
})

test_that("single period", {

  tbl1 <- tibble(a = list(NA, NA, "oil"), b = list(NA, 2010, NA),
                 c = list("a", sqrt(2), 2 * sqrt(2)), d =list("b", sqrt(3), NA))
  layout1 <- regts:::inspect_tibble(tbl1, frequency = NA, xlsx = TRUE)

  expected_result1 <- list(rowwise = FALSE, period_col = 2L,
                          first_data_row = 2L, last_data_col = 4L,
                          is_data_col = c(FALSE, FALSE, TRUE, TRUE),
                          is_data_row = c(FALSE, TRUE, FALSE),
                          names = c("a", "b"), lbls = NULL)
  expect_identical(layout1, expected_result1)

  tbl1_csv <- get_csv_tbl(tbl1)
  layout1_csv <- regts:::inspect_tibble(tbl1_csv, frequency = NA, xlsx = FALSE)

  expect_identical(layout1_csv, expected_result1)


  tbl2 <- tibble(a = list(NA, NA, "a", "b"), b = list(NA, "2010m2", sqrt(2),
                                                      sqrt(3)),
                 c = list("oil", sqrt(5), sqrt(6), sqrt(7)))

  layout2 <- regts:::inspect_tibble(tbl2, frequency = NA, xlsx = TRUE)

  expected_result2 <- list(rowwise = TRUE, period_row = 2L,
                          first_data_col = 2L, last_data_col = 2L,
                          is_data_col = c(FALSE, TRUE, FALSE),
                          periods = period("2010m2"))

  expect_identical(layout2, expected_result2)

  tbl3 <- tbl2[-4, ]

  wmsg <- paste("Could not determine if timeseries are stored rowwise or",
                "columnwise.\nFound a single period 2010m2. Assuming rowwise.")
  expect_warning(layout3 <- regts:::inspect_tibble(tbl3, frequency = NA,
                                                   xlsx = TRUE), wmsg)

  expected_result3 <- expected_result2
  expect_identical(layout3, expected_result3)

  tbl3_csv <- get_csv_tbl(tbl3)
  expect_warning(
    layout3_csv <- regts:::inspect_tibble(tbl3_csv, frequency = NA,
                                          xlsx = FALSE), wmsg)
  expect_identical(layout3, expected_result3)
})

test_that("missing data columns / rows ", {
  tbl1 <- tibble(a = list(NA, NA, "a", NA), b = list("xxx", 2010, 2011, 2012),
                 c = list(NA, NA, NA, NA))
  layout <- regts:::inspect_tibble(tbl1, frequency = NA, xlsx = TRUE)
  expect_identical(layout, list(rowwise = TRUE, period_row = 2L,
                               first_data_col = 2L, last_data_col = 2L,
                               is_data_col = c(FALSE, TRUE, FALSE),
                               periods = period(2010)))

  tbl2 <- tibble(a = c(NA, NA, "a", NA), b = c("xxx", "2010", "2011", "2012"),
                 c = c("y", NA, 3, NA))
  layout <- regts:::inspect_tibble(tbl2, frequency = NA, xlsx = FALSE,
                                   labels =  "after")
  expect_identical(layout, list(rowwise = FALSE, period_col = 2L,
                                first_data_row = 2L, last_data_col = 3L,
                                is_data_col = c(FALSE, FALSE, TRUE),
                                is_data_row = c(FALSE, rep(TRUE, 3)),
                                names = "y", lbls = NULL))

  layout_roww <- regts:::inspect_tibble(tbl2, frequency = NA, xlsx = FALSE,
                                   rowwise = TRUE)
  expect_identical(layout_roww, list(rowwise = TRUE, period_row = 2L,
                                first_data_col = 2L, last_data_col = 2L,
                                is_data_col = c(FALSE, TRUE, FALSE),
                                periods = period(2010)))


  tbl3 <- tibble(z = list("emu", NA, NA, NA), a = list(NA, NA, 2010, NA),
                 b = list("var a", "a", 2011, NA), c = list(NA, NA, 2012, NA))
  layout <- regts:::inspect_tibble(tbl3, frequency = NA, xlsx = TRUE,
                                   labels = "before")
  expect_identical(layout, list(rowwise = FALSE, period_col = 2L,
                                first_data_row = 3L, last_data_col = 3L,
                                is_data_col = c(FALSE, FALSE, TRUE, FALSE),
                                is_data_row = c(FALSE, FALSE, TRUE, FALSE),
                                names = "a", lbls = "var a"))
})

test_that("combination of numeric and text periods", {

  tbl1 <- tibble(a = list(NA, NA, "a", "b"),
                 b = list("x", "2010q1", 1, 2),
                 c = list("y", "2010q2", sqrt(2), sqrt(3)))
  expect_warning(layout1 <- regts:::inspect_tibble(tbl1, frequency = NA,
                                                   xlsx = TRUE), NA)

  expected_result1 <- list(rowwise = TRUE, period_row = 2L,
                           first_data_col = 2L, last_data_col = 3L,
                           is_data_col = c(FALSE, TRUE, TRUE),
                           periods = period(c("2010q1", "2010q2")))
  expect_identical(layout1, expected_result1)

  layout1_csv <-  regts:::inspect_tibble(get_csv_tbl(tbl1), frequency = NA,
                                         xlsx = FALSE)
  expect_identical(layout1_csv, expected_result1)


  tbl2 <- tibble(a = list(NA, "x", "y"), b = list(NA, "2010q1", "2010q2"),
                 c = list("a", 1, sqrt(2)), d = list("b", 2, sqrt(3)))

  expect_warning(layout2 <- regts:::inspect_tibble(tbl2, frequency = NA,
                                                   xlsx = TRUE), NA)

  expected_result2 <- list(rowwise = FALSE, period_col = 2L,
                           first_data_row = 2L, last_data_col = 4L,
                           is_data_col = c(FALSE, FALSE, TRUE, TRUE),
                           is_data_row = c(FALSE, TRUE, TRUE),
                           names = c("a", "b"), lbls = NULL)

  expect_identical(layout2, expected_result2)

  layout2_csv <-  regts:::inspect_tibble(get_csv_tbl(tbl2), frequency = NA,
                                         xlsx = FALSE)
  expect_identical(layout2_csv, expected_result2)


  tbl3 <- tibble(a = list(NA, "a", "b"), b = list("x", "2010q1", 12346),
                  c = list("y", 12345, sqrt(3)))

  expect_warning(
    layout3 <- regts:::inspect_tibble(tbl3, frequency = NA, xlsx = TRUE),
    wmsg_rw)

  expected_result3 <- expected_result1
  expected_result3$periods <- list(period("2010q1"), period(12345))
  expect_identical(layout3, expected_result3)

  # csv file cannot parse "12345" ...the function requires that a period
  # is present
  tbl3_csv <- tibble(a = c(NA, "a", "b"), b = c("x", "2010q1", 1234),
                 c = c("y", 1234, sqrt(3)))
  expect_warning(
    layout3_csv <-  regts:::inspect_tibble(tbl3_csv, frequency = NA,
                                         xlsx = FALSE), wmsg_rw)
  expected_result3_csv <- expected_result3
  expected_result3_csv$periods[[2]] <- period(1234)
  expect_identical(layout3_csv, expected_result3_csv)
})

test_that("all periods are numeric, check YYYY", {

  tbl1 <- tibble(a = list(NA, NA, "a", "b"),
                 b = list(NA, 2010, 1, 2),
                 c = list("x", 2011, 3, 4))

  expect_warning({
    layout1 <-  regts:::inspect_tibble(tbl1, frequency = NA, xlsx = TRUE)
    layout1_csv<-  regts:::inspect_tibble(get_csv_tbl(tbl1), frequency = NA,
                                          xlsx = FALSE)
  }, NA)

  expected_result1 <- list(rowwise = TRUE, period_row = 2L,
                           first_data_col = 2L, last_data_col = 3L,
                           is_data_col = c(FALSE, TRUE, TRUE),
                           periods = period(c("2010", "2011")))
  expect_identical(layout1, expected_result1)
  expect_identical(layout1_csv, expected_result1)


  tbl2 <- tibble(a = list(NA, NA, "a", "b"),
                 b = list(NA, 2010, sqrt(2), sqrt(3)),
                 c = list("x", 2011, sqrt(5), sqrt(7)))
  expect_warning({
    layout2 <-  regts:::inspect_tibble(tbl2, frequency = NA, xlsx = TRUE)
    layout2_csv <-  regts:::inspect_tibble(get_csv_tbl(tbl2), frequency = NA,
                                       xlsx = FALSE)
  }, NA)

  expected_result2 <- expected_result1
  expect_identical(layout2, expected_result2)
  expect_identical(layout2_csv, expected_result2)

  tbl3 <- tibble(a = list(NA, NA, "x"), b = list(NA, 2010, 2011),
                 c = list("a", 1, 2), d = list("b", 2, 4))
  expect_warning(
    layout3 <-  regts:::inspect_tibble(tbl3, frequency = NA, xlsx = TRUE), NA)

  expected_result3 <- list(rowwise = FALSE, period_col = 2L,
                           first_data_row = 2L, last_data_col = 4L,
                           is_data_col = c(FALSE, FALSE, TRUE, TRUE),
                           is_data_row = c(FALSE, TRUE, TRUE),
                           names = c("a", "b"), lbls = NULL)

  expect_identical(layout3, expected_result3)

  tbl4 <- tibble(a = list(NA, NA, "a", "b"),
                 b = list(NA, 22354, 11223, 2334),
                 c = list("x", 3223, 11131, -411))


  expect_warning(
    layout4 <- regts:::inspect_tibble(tbl4, frequency = NA, xlsx = TRUE),
    wmsg_cw)

  expected_result4 <- list(rowwise = FALSE, period_col = 2L,
                           first_data_row = 2L, last_data_col = 3L,
                           is_data_col = c(FALSE, FALSE, TRUE),
                           is_data_row = c(FALSE, TRUE, TRUE, TRUE),
                           names = "x", lbls = NULL)
})

test_that("all periods are numeric, check YY periods", {

  tbl1 <- tibble(a = list(NA, NA, "a", "b"),
                 b = list(NA, 18, 112445, -22),
                 c = list("x", 19, -33, 42134))

  expect_warning(
    layout1 <-  regts:::inspect_tibble(tbl1, frequency = NA, xlsx = TRUE), NA)

  expected_result1 <- list(rowwise = TRUE, period_row = 2L,
                           first_data_col = 2L, last_data_col = 3L,
                           is_data_col = c(FALSE, TRUE, TRUE),
                           periods = period(18:19))
  expect_identical(layout1, expected_result1)
})

test_that("weird periods in first column", {

  tbl1 <- tibble(b = list("2010", "2010q1", 1, 2),
                c = list(NA, "2010q2", 1, 2),
                d = list(NA, "2010q3", 1, 2))

  layout1 <- regts:::inspect_tibble(tbl1, frequency = NA, xlsx = TRUE)

  expected_result1 <- list(rowwise = FALSE, period_col = 1L,
                           first_data_row = 1L, last_data_col = NA_real_,
                           is_data_col = c(FALSE, FALSE, FALSE),
                           is_data_row = c(TRUE, TRUE, TRUE, TRUE),
                           names = character(0), lbls = NULL)
  expect_equal(layout1, expected_result1)

  tbl1_csv <- tibble(b = c("2010", "2010q1", 1, 2),
                 c = c(NA, "2010q2", 1, 2),
                 d = c(NA, "2010q3", 1, 2))

  layout1_csv <-  regts:::inspect_tibble(tbl1_csv, frequency = NA,
                                         xlsx = FALSE)
  expect_equal(layout1_csv, expected_result1)

  tbl2 <- tibble(b = list("2010", "2010q1", 1, 2),
                 c = list(NA, "2010q2", 1, 2),
                 d = list("x", "2010q3", 1, 2))

  layout2 <- regts:::inspect_tibble(tbl2, frequency = NA, xlsx = TRUE)


  expected_result2 <- list(rowwise = FALSE, period_col = 1L,
                           first_data_row = 2L, last_data_col = 3,
                           is_data_col = c(FALSE, FALSE, TRUE),
                           is_data_row = c(FALSE, TRUE, TRUE, TRUE),
                           names = "x", lbls = NULL)

  expect_equal(layout2, expected_result2)

  tbl2_csv <- tibble(b = c("2010", "2010q1", 1, 2),
                     c = c(NA, "2010q2", 1, 2),
                     d = c("x", "2010q3", 1, 2))

  layout2_csv <-  regts:::inspect_tibble(tbl2_csv, frequency = NA,
                                         xlsx = FALSE)
  expect_equal(layout2_csv, expected_result2)
})
