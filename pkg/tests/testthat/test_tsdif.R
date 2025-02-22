library(regts)
library(testthat)

rm(list = ls())

update_expected <- FALSE

# Functions --------------------------------------------------------------------

create_tsdif <- function(...) {
  return(structure(list(...), class = "tsdif"))
}

get_dif_table <- function(ts1, ts2, dif, tol = 0) {
  names <- colnames(dif)
  ts1_long <- as.data.frame(ts1[, names, drop = FALSE], long = TRUE)
  ts2_long <- as.data.frame(ts2[, names, drop = FALSE], long = TRUE)
  dif_long <- as.data.frame(dif, long = TRUE)

  tot <- merge(dif_long, ts1_long, by = c("name", "period")) |>
    merge(ts2_long, by = c("name", "period"))

  colnames(tot)[3:5] <- c("dif", "value1", "value2")

  tot <- tot[is.na(dif) | abs(dif) > tol, ]

  rownames(tot) <- NULL

  return(tot)
}

# Prepare input data -----------------------------------------------------------

ts1 <- regts(matrix(data = rep(1:9), nc = 3),
  start = "2008Q4",
  names = c("a", "b", "c")
)
ts2 <- ts1 + 0.01
colnames(ts2) <- c("a", "b", "d")
ts2["2008Q3", ] <- 2
ts1["2009Q3", ] <- 2

difference <- regts(matrix(data = rep(-0.01, 6), nc = 2),
  start = "2008Q4",
  names = c("a", "b")
)

maxdif <- data.frame(
  period = "2008Q4",
  maxdif = rep(-0.01, 2),
  value1 = c(1, 4),
  value2 = c(1.01, 4.01),
  row.names = c("a", "b")
)

res_correct <- create_tsdif(
  equal = FALSE, difnames = c("a", "b"),
  dif = difference,
  dif_table = get_dif_table(ts1, ts2, difference),
  maxdif = maxdif,
  common_names = c("a", "b"),
  missing_names1 = "d", missing_names2 = "c",
  common_range = period_range("2008Q4/2009Q2"),
  period_range1 = get_period_range(ts1),
  period_range2 = get_period_range(ts2),
  ranges_equal = FALSE, ts_names = c("ts1", "ts2"),
  tol = 0, fun = NULL
)

# Tests ------------------------------------------------------------------------

test_that("simple example", {
  res <- tsdif(ts1, ts2)
  expect_equal(res, res_correct)
})

test_that("labels", {
  ts1_lbls <- ts1
  ts2_lbls <- ts2
  ts_labels(ts1_lbls) <- c("x", "y", "z")
  ts_labels(ts2_lbls) <- c("p", "q", "r")

  res <- tsdif(ts1_lbls, ts2_lbls)

  # Only component dif contains labels. dif_table and maxdif do not have
  # labels.
  res_expected <- res
  ts_labels(res_expected$dif) <- c("x", "y")
  expect_equal(res, res_expected)
})

test_that("argument fun", {
  res2 <- tsdif(ts1, ts2, fun = function(x1, x2) (sin(x2 - x1)))
  res2_correct <- res_correct
  res2_correct$fun <- "function(x1, x2) (sin(x2 - x1))"
  res2_correct$dif <- sin(-res2_correct$dif)
  res2_correct$dif_table <- get_dif_table(ts1, ts2, res2_correct$dif)
  res2_correct$maxdif$maxdif <- sin(-res2_correct$maxdif$maxdif)
  expect_equal(res2, res2_correct)
})

test_that("check simple output", {
  expect_known_output(tsdif(ts1, ts2), "expected_output/tsdif.txt",
    print = TRUE, update = update_expected
  )
})

test_that("no difference", {
  res <- tsdif(ts1, ts1)
  res_no_dif <- create_tsdif(
    equal = TRUE, difnames = character(0),
    dif = NULL, dif_table = NULL, maxdif = NULL,
    common_names = c("a", "b", "c"),
    missing_names1 = character(0),
    missing_names2 = character(0),
    common_range = get_period_range(ts1),
    period_range1 = get_period_range(ts1),
    period_range2 = get_period_range(ts1),
    ranges_equal = TRUE,
    ts_names = c("ts1", "ts1"),
    tol = 0, fun = NULL
  )
  expect_equal(res, res_no_dif)
  expect_known_output(res, "expected_output/tsdif_no_dif.txt",
    print = TRUE, update = update_expected
  )
})

test_that("univariate timeseries", {
  # univariate timeseries without column name
  res2 <- tsdif(ts1[, "a"], ts2[, "a"])

  var_name <- "ts_without_name"
  res_correct2 <- res_correct
  res_correct2[c("difnames", "common_names")] <- var_name
  res_correct2$missing_names1 <- character(0)
  res_correct2$missing_names2 <- character(0)
  dif <- res_correct$dif[, "a", drop = FALSE]

  colnames(dif) <- var_name
  dif_table <- get_dif_table(
    regts:::univec2unimat(ts1[, "a"], var_name),
    regts:::univec2unimat(ts2[, "a"], var_name),
    dif
  )

  maxdif <- res_correct$maxdif[1, , drop = FALSE]
  rownames(maxdif) <- var_name

  res_correct2$dif <- dif
  res_correct2$dif_table <- dif_table
  res_correct2$maxdif <- maxdif
  res_correct2$ts_names <- c("ts1[, \"a\"]", "ts2[, \"a\"]")
  expect_equal(res2, res_correct2)

  # univariate timeseries with column name
  res3 <- tsdif(ts1[, "a", drop = FALSE], ts2[, "a", drop = FALSE])

  res_correct3 <- res_correct
  res_correct3[c("difnames", "common_names")] <- "a"
  res_correct3$missing_names1 <- character(0)
  res_correct3$missing_names2 <- character(0)
  dif <- res_correct$dif[, "a", drop = FALSE]
  dif_table <- get_dif_table(ts1, ts2, dif)

  maxdif <- res_correct$maxdif[1, , drop = FALSE]
  res_correct3$dif <- dif
  res_correct3$dif_table <- dif_table
  res_correct3$maxdif <- maxdif
  res_correct3$ts_names <- c(
    "ts1[, \"a\", drop = FALSE]",
    "ts2[, \"a\", drop = FALSE]"
  )
  expect_equal(res3, res_correct3)

  wmsg <- paste(
    "Timeseries ts1[, \"a\", drop = FALSE] and ts2[, \"b\", drop = FALSE] have",
    "no common columns"
  )
  expect_warning(
    res4 <- tsdif(ts1[, "a", drop = FALSE], ts2[, "b", drop = FALSE]),
    wmsg,
    fixed = TRUE
  )

  res_correct4 <- res_correct
  res_correct4$difnames <- character(0)
  res_correct4$common_names <- character(0)
  res_correct4$missing_names1 <- "b"
  res_correct4$missing_names2 <- "a"
  res_correct4["dif_table"] <- list(NULL)
  res_correct4["dif"] <- list(NULL)
  res_correct4["maxdif"] <- list(NULL)
  res_correct4$ts_names <- c(
    "ts1[, \"a\", drop = FALSE]",
    "ts2[, \"b\", drop = FALSE]"
  )
  expect_equal(res4, res_correct4)
})

test_that("errors", {
  expect_error(tsdif(ts1[, "a"], 2),
    "Argument x2 (2) is not a timeseries",
    fixed = TRUE
  )
  expect_error(tsdif(res_correct, ts1[, "a"]),
    "Argument x1 (res_correct) is not a timeseries",
    fixed = TRUE
  )

  msg <- "Argument tol should be >= 0"
  expect_error(tsdif(ts1, ts1, tol = -1), msg)

  # different frequencies
  tsy <- regts(matrix(data = rep(1:9), nc = 3),
    start = "2008",
    names = c("a", "b", "c")
  )
  expect_error(
    tsdif(ts1, tsy),
    "Timeseries x1 and x2 \\(ts1 and tsy\\) have different frequencies"
  )
})

test_that("only one NA difference", {
  ts1_na <- ts1
  ts1_na[2, 2] <- NA
  res <- tsdif(ts1, ts1_na, tol = 0)

  dif <- zero_trim((ts1 - ts1_na)[, "b", drop = FALSE])

  dif_table <- get_dif_table(ts1, ts1_na, dif)

  maxdif <- data.frame(
    period = as.character(get_periods(ts1)[2]),
    maxdif = NA_real_,
    value1 = ts1[2, 2],
    value2 = ts1_na[2, 2],
    row.names = colnames(ts1)[2]
  )

  res_no_dif <- create_tsdif(
    equal = FALSE, difnames = "b",
    dif = dif, dif_table = dif_table, maxdif = maxdif,
    common_names = c("a", "b", "c"),
    missing_names1 = character(0),
    missing_names2 = character(0),
    common_range = get_period_range(ts1),
    period_range1 = get_period_range(ts1),
    period_range2 = get_period_range(ts1),
    ranges_equal = TRUE,
    ts_names = c("ts1", "ts1_na"),
    tol = 0, fun = NULL
  )
  expect_equal(res, res_no_dif)
})

test_that("two NA differences", {
  ts1_na <- ts1
  ts1_na[2, 2] <- NA
  ts1_na[4, 3] <- NA
  res <- tsdif(ts1, ts1_na, tol = 0)

  dif <- zero_trim((ts1 - ts1_na)[, -1])

  dif_table <- get_dif_table(ts1, ts1_na, dif)

  maxdif <- data.frame(
    period = as.character(get_periods(dif))[c(1, 3)],
    maxdif = NA_real_,
    value1 = c(ts1[2, 2], ts1[4, 3]),
    value2 = c(ts1_na[2, 2], ts1_na[4, 3]),
    row.names = colnames(dif)
  )

  res_no_dif <- create_tsdif(
    equal = FALSE, difnames = c("b", "c"),
    dif = dif, dif_table = dif_table, maxdif = maxdif,
    common_names = c("a", "b", "c"),
    missing_names1 = character(0),
    missing_names2 = character(0),
    common_range = get_period_range(ts1),
    period_range1 = get_period_range(ts1),
    period_range2 = get_period_range(ts1),
    ranges_equal = TRUE,
    ts_names = c("ts1", "ts1_na"),
    tol = 0, fun = NULL
  )
  expect_equal(res, res_no_dif)
})

test_that("differences smaller than tol", {
  res <- tsdif(ts1, ts2, tol = 0.1)
  res_tol <- res_correct
  res_tol$tol <- 0.1
  res_tol["dif_table"] <- list(NULL)
  res_tol["dif"] <- list(NULL)
  res_tol["maxdif"] <- list(NULL)
  res_tol$difnames <- character()
  expect_equal(res, res_tol)

  # now use the sample without NA values, there should be no differences
  sample <- period_range("2008Q4", "2009Q2")
  res2 <- tsdif(ts1[sample, ], ts2[sample, ], tol = 0.1)
  res2_tol <- res_tol
  res2_tol$difnames <- character(0)
  res2_tol["dif_table"] <- list(NULL)
  res2_tol["dif"] <- list(NULL)
  res2_tol$common_range <- sample
  res2_tol$period_range1 <- sample
  res2_tol$period_range2 <- sample
  res2_tol$ranges_equal <- TRUE
  res2_tol$ts_names <- c("ts1[sample, ]", "ts2[sample, ]")
  expect_equal(res2, res2_tol)
})

test_that("single period", {
  res <- tsdif(ts1["2008Q4", ], ts2["2008Q4", ])
  res_correct2 <- res_correct
  res_correct2$dif <- difference["2008Q4", ]
  res_correct2$dif_table <- get_dif_table(ts1, ts2, res_correct2$dif)
  res_correct2$common_range <- period_range("2008Q4", "2008Q4")
  res_correct2$period_range1 <- period_range("2008Q4", "2008Q4")
  res_correct2$period_range2 <- period_range("2008Q4", "2008Q4")
  res_correct2$ranges_equal <- TRUE
  res_correct2$ts_names <- c("ts1[\"2008Q4\", ]", "ts2[\"2008Q4\", ]")
  expect_equal(res, res_correct2)
})

test_that("no common columns", {
  x2 <- ts2
  colnames(x2) <- toupper(colnames(ts2))
  msg <- "Timeseries ts1 and x2 have no common columns"
  expect_warning(res <- tsdif(ts1, x2), msg)
  res_correct2 <- create_tsdif(
    equal = FALSE, difnames = character(0),
    dif = NULL, dif_table = NULL, maxdif = NULL,
    common_names = character(0),
    missing_names1 = c("A", "B", "D"),
    missing_names2 = c("a", "b", "c"),
    common_range = period_range("2008Q4/2009Q2"),
    period_range1 = get_period_range(ts1),
    period_range2 = get_period_range(x2),
    ranges_equal = FALSE,
    ts_names = c("ts1", "x2"),
    tol = 0, fun = NULL
  )
  expect_equal(res, res_correct2)
})

test_that("duplicate column names", {
  x2 <- ts2
  colnames(x2)[2] <- "a"
  msg <- "Duplicate column names in timeseries x2"
  expect_error(tsdif(ts1, x2), msg)
  expect_error(tsdif(x2, ts1), msg)
})


test_that("single ts as result", {
  sample <- period_range("2008Q4", "2009Q2")
  tol <- 0.1

  x2 <- ts2[sample, ]
  x2[sample, "b"] <- ts1[sample, "b"] + 0.11
  res <- tsdif(ts1[sample, ], x2, tol = tol)

  dif <- regts(matrix(data = rep(-0.11, 3), nc = 1),
               start = "2008Q4",
               names = c("b"))
  dif_table <- get_dif_table(ts1, x2, dif, tol = tol)

  res_correct2 <- res_correct
  res_correct2$tol <- 0.1
  res_correct2$difnames <- "b"
  res_correct2$dif <- dif
  res_correct2$dif_table <- dif_table

  prd <- "2008Q4"
  var_name <- "b"
  res_correct2$maxdif <- data.frame(
    period = prd,
    maxdif = -0.11,
    value1 = as.numeric(ts1[prd, var_name]),
    value2 = as.numeric(x2[prd, var_name]),
    row.names = var_name
  )

  res_correct2$ts_names <- c("ts1[sample, ]", "x2")
  res_correct2$period_range1 <- sample
  res_correct2$period_range2 <- sample
  res_correct2$ranges_equal <- TRUE

  expect_equal(res, res_correct2)
})

test_that("single common column", {
  res <- tsdif(ts1[, c("a", "c")], ts2[, c("d", "a")])

  prd <- "2008Q4"
  var_name <- "a"

  dif <- regts(matrix(data = rep(-0.01, 3), nc = 1),
    start = prd, names = var_name
  )

  dif_table <- get_dif_table(ts1, ts2, dif)

  maxdif <- data.frame(
    period = prd,
    maxdif = -0.01,
    value1 = as.numeric(ts1[prd, var_name]),
    value2 = as.numeric(ts2[prd, var_name]),
    row.names = var_name
  )

  res_correct2 <- create_tsdif(
    equal = FALSE, difnames = c("a"),
    dif = dif, dif_table = dif_table, maxdif = maxdif,
    common_names = "a",
    missing_names1 = "d", missing_names2 = "c",
    common_range = period_range("2008Q4/2009Q2"),
    period_range1 = get_period_range(ts1),
    period_range2 = get_period_range(ts2),
    ranges_equal = FALSE,
    ts_names = c(
      "ts1[, c(\"a\", \"c\")]",
      "ts2[, c(\"d\", \"a\")]"
    ),
    tol = 0, fun = NULL
  )
  expect_equal(res, res_correct2)
})

test_that("no column names simple", {
  x <- ts1
  y <- ts2[, 1:2]
  colnames(x) <- NULL
  colnames(y) <- NULL
  res <- tsdif(x, y)

  difference3 <- difference[, 1:2]
  colnames(difference3) <- c("column 1", "column 2")

  x_cnames <- x
  y_cnames <- y
  colnames(x_cnames) <- paste("column", 1:3)
  colnames(y_cnames) <- paste("column", 1:2)
  dif_table <- get_dif_table(x_cnames, y_cnames, difference3)

  maxdif3 <- data.frame(
    period = "2008Q4",
    maxdif = rep(-0.01, 2),
    value1 = c(1, 4),
    value2 = c(1.01, 4.01),
    row.names = colnames(difference3)
  )

  res_correct3 <- create_tsdif(
    equal = FALSE,
    difnames = paste("column", 1:2),
    dif = difference3, dif_table = dif_table, maxdif = maxdif3,
    common_names = paste("column", 1:2),
    missing_names1 = character(0),
    missing_names2 = "column 3",
    common_range = period_range("2008Q4", "2009Q2"),
    period_range1 = get_period_range(x),
    period_range2 = get_period_range(y),
    ranges_equal = FALSE,
    ts_names = c("x", "y"),
    tol = 0, fun = NULL
  )
  expect_equal(res, res_correct3)
})

test_that("no overlapping periods", {
  msg <- paste(
    "Timeseries ts1[\"2008Q4/2009Q1\", ] and ts2[\"2009Q2\", ] have no common",
    "period range"
  )
  expect_warning(
    res <- tsdif(ts1["2008Q4/2009Q1", ], ts2["2009Q2", ]),
    msg,
    fixed = TRUE
  )
  res_correct2 <- res_correct
  res_correct2["dif"] <- list(NULL)
  res_correct2["dif_table"] <- list(NULL)
  res_correct2["maxdif"] <- list(NULL)
  res_correct2$difnames <- character(0)
  res_correct2["common_range"] <- list(NULL)
  res_correct2$period_range1 <- period_range("2008Q4", "2009Q1")
  res_correct2$period_range2 <- period_range("2009Q2", "2009Q2")
  res_correct2$ranges_equal <- FALSE
  res_correct2$ts_names <- c("ts1[\"2008Q4/2009Q1\", ]", "ts2[\"2009Q2\", ]")
  expect_equal(res, res_correct2)
})

test_that("a combination of negative and positive differences", {
  ts1 <- regts(matrix(data = rep(1:9), nc = 3),
    start = "2008Q4",
    names = c("a", "b", "c")
  )
  ts2 <- ts1
  ts2[, "a"] <- ts1[, "a"] + c(-0.2, 0, 0.1)
  dif_correct <- ts2[, "a", drop = FALSE] - ts1[, "a", drop = FALSE]
  dif1 <- tsdif(ts2, ts1)
  expect_equal(dif1$dif, dif_correct)
  dif2 <- tsdif(ts1, ts2)
  expect_equal(dif2$dif, -dif_correct)
  dif3 <- tsdif(ts2, ts1, tol = 0.15)
  expect_equal(dif3$dif, dif_correct["2008Q4"])
  dif4 <- tsdif(ts1, ts2, tol = 0.15)
  expect_equal(dif4$dif, -dif_correct["2008Q4"])
})

test_that("NA, NaN, Inf and -Inf values", {
  ts1 <- regts(matrix(data = rep(NA, 4), nc = 2),
    start = "2016",
    names = c("a", "b")
  )
  ts2 <- ts1 + 0.01
  colnames(ts2) <- c("a", "b")

  dif1 <- tsdif(ts2, ts1)
  expect_equal(dif1$equal, TRUE)

  tsn1 <- regts(matrix(data = rep(NaN, 4), nc = 2),
    start = "2016",
    names = c("a", "b")
  )
  tsn2 <- tsn1 + 0.01
  colnames(tsn2) <- c("a", "b")

  dif1 <- tsdif(tsn2, tsn1)
  expect_equal(dif1$equal, TRUE)

  ts1 <- regts(matrix(data = rep(1:4) / 0, nc = 2),
    start = "2016",
    names = c("a", "b")
  )
  ts2 <- ts1 + 0.01
  colnames(ts2) <- c("a", "b")
  dif1 <- tsdif(ts2, ts1)
  expect_equal(dif1$equal, TRUE)
  dif1 <- tsdif(ts2, ts1, fun = cvgdif)
  expect_equal(dif1$equal, TRUE)

  ts1 <- regts(matrix(data = -rep(1:4) / 0, nc = 2),
    start = "2016",
    names = c("a", "b")
  )
  ts2 <- ts1 + 0.01
  dif1 <- tsdif(ts2, ts1)
  expect_equal(dif1$equal, TRUE)

  dif1 <- tsdif(ts2, ts1, fun = cvgdif)
  expect_equal(dif1$equal, TRUE)

  # Compare Inf and -Inf: result is Inf
  ts2$b <- -ts2$b
  dif1 <- tsdif(ts2, ts1)
  expect_equal(dif1$dif, regts(matrix(c(Inf, Inf), ncol = 1),
    names = "b",
    start = "2016"
  ))

  # Compare Inf and -Inf with function cvgdif: result is NaN
  dif1 <- tsdif(ts2, ts1, fun = cvgdif)
  expect_equal(dif1$dif, regts(matrix(c(NaN, NaN), ncol = 1),
    names = "b",
    start = "2016"
  ))
})

test_that("test combinations of NA, NaN, Inf and proper values", {
  ts1 <- regts(matrix(data = c(NA, NA, NA, NA, NaN, NaN), nc = 3),
    start = "2016",
    names = c("a", "b", "c")
  )
  ts2 <- regts(matrix(data = c(NaN, NaN, rep(1:4)), nc = 3),
    start = "2016",
    names = c("a", "b", "c")
  )

  dif <- ts1

  dif_table <- get_dif_table(ts1, ts2, dif)

  dif_prd <- get_periods(dif)[1]
  maxdif <- data.frame(
    period = as.character(dif_prd),
    maxdif = dif[1, ],
    value1 = as.numeric(ts1[dif_prd, colnames(dif)]),
    value2 = as.numeric(ts2[dif_prd, colnames(dif)]),
    row.names = colnames(dif)
  )

  res_dif <- create_tsdif(
    equal = FALSE, difnames = c("a", "b", "c"),
    dif = dif, dif_table = dif_table, maxdif = maxdif,
    common_names = c("a", "b", "c"),
    missing_names1 = character(0),
    missing_names2 = character(0),
    common_range = get_period_range(ts1),
    period_range1 = get_period_range(ts1),
    period_range2 = get_period_range(ts1),
    ranges_equal = TRUE,
    ts_names = c("ts1", "ts2"),
    tol = 0, fun = NULL
  )

  dif1 <- tsdif(ts1, ts2)
  expect_equal(dif1, res_dif)

  ts_inf <- regts(matrix(
    data = c(rep(1 / 0, 2), rep(-1 / 0, 2), rep(1 / 0, 2), rep(-1 / 0, 2)),
    nc = 2
  ), start = "2016", names = c("a", "b"))
  ts_na <- regts(matrix(
    data = c(rep(NA, 4), rep(NaN, 4)),
    nc = 2
  ), start = "2016", names = c("a", "b"))
  expect_equal(tsdif(ts_inf, ts_na)$dif, ts_na)
})

test_that("check more complex case with combinations of NA and proper values", {
  ts1 <- regts(matrix(data = c(1:144), nc = 12),
    start = "2016q1",
    names = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
  )
  ts2 <- (ts1 + 1) + 2 * ts1
  ts1["2016q3", "c"] <- NA
  ts2["2017q4", "f"] <- NA
  expect_known_output(tsdif(ts1, ts2), "expected_output/tsdif_complex.txt",
    print = TRUE, update = update_expected
  )
  max_difnames_old <- getOption("regts_max_difnames")
  max_maxdif_old <- getOption("regts_max_maxdif")
  on.exit({
    options(list(
      regts_max_difnames = max_difnames_old,
      regts_max_maxdif = max_maxdif_old
    ))
  })
  expect_equal(max_difnames_old, 50)
  expect_equal(max_maxdif_old, 10)
  options(list(regts_max_difnames = 10, regts_max_maxdif = 12))
  expect_equal(getOption("regts_max_difnames"), 10)
  expect_equal(getOption("regts_max_maxdif"), 12)

  expect_known_output(tsdif(ts1, ts2),
    "expected_output/tsdif_complex_options.txt",
    print = TRUE, update = update_expected
  )
})

test_that("function cvgdif", {
  expect_equal(cvgdif(0.12, 0.14), 0.02)
  expect_equal(cvgdif(0.12, -0.14), 0.26)
  expect_equal(cvgdif(-0.12, 8.0), 8.12 / 8)
  expect_equal(cvgdif(-0.12, -8.0), 7.88 / 8)
})
