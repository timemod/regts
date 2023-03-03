
test_that("constructor regts for univariate timeseries", {

    regts1 <- regts(1:10, start = "2010Q4", labels = "Timeseries a")
    res <- "Timeseries a"
    expect_identical(ts_labels(regts1), res)

    expect_error(update_ts_labels(regts1, c(a = "ts a", x = "???")),
                 paste("x does not have column names. update_labels requires a",
                       "regts object with named columns"))
})

test_that("constructor regts for univariate matrix timeseries", {

    regts1 <- regts(matrix(1:10, nc = 1), start = "2010Q4", names = "a",
                    labels = "Timeseries a")
    res <- "Timeseries a"
    names(res) <- "a"
    expect_identical(ts_labels(regts1), res)

    regts2 <- update_ts_labels(regts1, c(a = "ts a", x = "???"))
    res2 <- res
    res2['a'] <- "ts a"
    expect_identical(ts_labels(regts2), res2)
})

test_that("constructor regts for univariate character timeseries", {

    regts1 <- regts(paste0("text", as.character(1:10)), start = "2010Q4",
                           labels = "Timeseries a")
    res <- "Timeseries a"
    expect_identical(ts_labels(regts1), res)
})

test_that("constructor regts for multivariate timeseries", {

    regts1 <- regts(matrix(rep(1:10), ncol = 2), start = "2010Q4",
                    names = c("a", "b"),
                    labels = c("Timeseries a", "Timeseries b"))
    res <- c("Timeseries a", "Timeseries b")
    names(res) <- c("a", "b")
    expect_identical(ts_labels(regts1), res)

    regts2 <- update_ts_labels(regts1, c(a = "ts a", x = "???"))
    res2 <- res
    res2['a'] <- "ts a"
    expect_identical(ts_labels(regts1), res)

    expect_identical(ts_labels(regts1[, 1, drop = FALSE]), res['a'])
    expect_identical(ts_labels(regts1[, 'b']), unname(res['b']))
    expect_identical(ts_labels(regts1[, c('a', 'b')]), res)
    expect_identical(ts_labels(regts1[, c('b', 'a')]), res[c("b", "a")])

    regts2 <- update_ts_labels(regts1, NULL)
    expect_identical(ts_labels(regts2), NULL)

    # names and labels as factors
    regts3 <- regts(matrix(rep(1:10), ncol = 2), start = "2010Q4",
                    names = as.factor(c("a", "b")),
                    labels = as.factor(c("Timeseries a", "Timeseries b")))
    expect_identical(ts_labels(regts1), ts_labels(regts3))
    expect_identical(regts1, regts3)
})


test_that("adding labels to a regts", {
    regts1 <- regts(matrix(rep(1:10), ncol = 2), start = "2010Q4",
                    names = c("a", "b"))
    ts_labels(regts1) <- c("ta", "tb")
    regts1 <- update_ts_labels(regts1, c(a = "Timeseries a"))
    regts2 <- regts(matrix(rep(1:10), ncol = 2), start = "2010Q4",
                    names = c("a", "b"),
                    labels = c("Timeseries a", "tb"))
    expect_identical(regts1, regts2)
})

test_that("adding a column to a regts", {
    regts1 <- regts(matrix(rep(1:10), ncol = 2), start = "2010Q4",
                    names = c("a", "b"),
                    labels = c("Timeseries a", "Timeseries b"))
    regts1['2012Q2', 'x'] <- 2

    ref <- c("Timeseries a", "Timeseries b", "")
    names(ref) <- colnames(regts1)

    expect_identical(ts_labels(regts1), ref)

    regts2 <- update_ts_labels(regts1, c(x = "Timeseries x"))
    ref2 <- c("Timeseries a", "Timeseries b", "Timeseries x")
    names(ref2) <- colnames(regts2)
    expect_identical(ts_labels(regts2), ref2)
})

test_that("error messages", {
    expect_error(regts(matrix(1:4, ncol = 2), start = "2010", names = c("a","b"),
                       labels = c(1,2)), "value should be a character vector")
    expect_error(regts(matrix(1:4, ncol = 2), start = "2010", names = c("a","b"),
                       labels = c("1","2","3")),
                 "The length of the labels argument should be equal to the number of columns")
    regts1 <- regts(matrix(1:4, ncol = 2), start = "2010")
    expect_error(update_ts_labels(regts1, labels = c("a","b")),
                 "x does not have column names. update_labels requires a regts object with named columns")
})

test_that("labels are preserved in miscellaneous timeseries functions (uni)", {
    x <- regts(1:10, start = "2010Q4", labels = "Timeseries a")
    x_sin <- sin(x)
    x_lag <- lag(x)
    x_diff <- diff(x)
    x_agg <- aggregate(x)
    x_agg_gr <- aggregate_gr(x, method = "difmean")
    x_windows <- window(x, start = c(2011, 4))
    expect_identical(ts_labels(x), ts_labels(x_sin))
    expect_identical(ts_labels(x), ts_labels(x_lag))
    expect_identical(ts_labels(x), ts_labels(x_diff))
    expect_identical(ts_labels(x), ts_labels(x_agg))
    expect_identical(ts_labels(x), ts_labels(x_agg_gr))
    expect_identical(ts_labels(x), ts_labels(x_windows))
})

test_that("labels are preserved in miscellaneous timeseries functions (multi)", {
    x <- regts(matrix(1:20, nc = 2), start = "2010Q4",
               labels = c("Timeseries a", "Timeseries b"))
    x_sin <- sin(x)
    x_lag <- lag(x)
    x_diff <- diff(x)
    x_agg <- aggregate(x)
    x_agg_gr <- aggregate_gr(x, method = "difmean")
    x_windows <- window(x, start = c(2011, 4))
    expect_identical(ts_labels(x), ts_labels(x_sin))
    expect_identical(ts_labels(x), ts_labels(x_lag))
    expect_identical(ts_labels(x), ts_labels(x_diff))
    expect_identical(ts_labels(x), ts_labels(x_agg))
    expect_identical(ts_labels(x), ts_labels(x_agg_gr))
    expect_identical(ts_labels(x), ts_labels(x_windows))
})

test_that("column selection in a timeseries with 1 row and labels", {
    # selecting more than one column in a timeseries with 1 row has an odd
    # result for class ts if drop = TRUE. This behaviour is corrected for
    # in regts (see the implementation of "[.regts")

    regts1 <- regts(matrix(1:3, nc = 3), "2010Q2", names = c("a", "b", "c"),
                    labels = c("Var a", "Var b", "Var c"))

    # select two columns
    ref <- regts(matrix(2:3, nc = 2), "2010Q2", names = c("b", "c"),
                 labels = c("Var b", "Var c"))
    expect_identical(regts1[, c("b", "c")], ref)

    # select one column (regts1[, "c"] is a named vector)
    ref <- regts(3, "2010Q2", labels = "Var c")
    names(ref) <- "c"
    expect_equal(regts1[, "c"], ref)
})

test_that("labels ok after renaming columns", {

  regts1 <- regts(matrix(rep(1:10), ncol = 2), start = "2010Q4",
                  names = c("A", "B"),
                  labels = c("Timeseries a", "Timeseries b"))
  colnames(regts1) <- tolower(colnames(regts1))

  res <- c("Timeseries a", "Timeseries b")
  names(res) <- c("a", "b")
  expect_identical(ts_labels(regts1), res)

  regts2 <- update_ts_labels(regts1, labels = c(b = "Variable b"))
  res2 <- res
  res2["b"] <- "Variable b"
  expect_identical(ts_labels(regts2), res2)

  sel <- regts1[, "b", drop = FALSE]
  expect_identical(ts_labels(sel), res["b"])
})

test_that("read labels from file, this can be factors", {
  ts <- regts(matrix(1:4, ncol=2), names = c("c","b"), start = "2016")

  # read labels as characters
  dflabel <- read.csv("csv/label.csv", stringsAsFactors = FALSE)
  labels <- dflabel[[2]]
  names(labels) <- dflabel[[1]]
  ts1 <- update_ts_labels(ts, labels)

  # read labels as factors
  dflabel <- read.csv("csv/label.csv")
  labels <- dflabel[[2]]
  names(labels) <- dflabel[[1]]
  ts2 <- update_ts_labels(ts, labels)

  expect_identical(ts1, ts2)

})
