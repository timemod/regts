context("convert_selection_range")

test_that("monthly timeseries", {
    r <- period_range("2010Q4", "2011Q3")
    r <- regts:::convert_selection_range(r, period_range("2010M1", "2011M11"))
    expect_identical(r, as.numeric(as.period_range("2010M10/2011M9")))
    r <- period_range("2010Q4", NULL)
    r <- regts:::convert_selection_range(r, period_range("2010M1", "2011M11"))
    expect_identical(r, as.numeric(as.period_range("2010M10/2011M11")))
    r <- period_range(NULL, "2010")
    r <- regts:::convert_selection_range(r, period_range("2010M2", "2011M11"))
    expect_identical(r, as.numeric(as.period_range("2010M2/2010M12")))
})
