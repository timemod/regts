
test_that("t.regts for univariate timeseries", {
  data <- 1:4
  rts1 <- regts(data, start = "2010Q2")
  expect_identical(t(rts1), matrix(data, nrow = 1))
})

test_that("t.regts for univariate timeseries", {
  data <- matrix(1:4, ncol = 2)
  colnames(data) <- c("a", "b")
  rts1 <- regts(data, start = "2010Q2")
  expect_identical(t(rts1), t(data))
})

