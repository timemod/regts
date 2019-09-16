library(regts)

context("test for duplicate names")

test_that("example_duplicate_names.xlsx has duplicate names",  {
xlsx_file <- "xlsx/example_duplicate_names.xlsx"
warnings <- capture_warnings(result <- read_ts_xlsx(xlsx_file, strict = FALSE))

expect_identical(warnings,
c("Duplicate names on sheet 1 of file xlsx/example_duplicate_names.xlsx: b, a\n"))
})
