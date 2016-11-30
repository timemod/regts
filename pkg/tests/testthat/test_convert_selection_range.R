context("convert_selection_range")

test_that("monthly timeseries", {
    r <- regperiod_range("2010Q4", "2011Q3")
    r <- regts:::convert_selection_range(r, regperiod_range("2010M1", "2011M11"))
    expect_identical(r, as.numeric(as.regperiod_range("2010M10/2011M9")))
    r <- regperiod_range("2010Q4", NULL)
    r <- regts:::convert_selection_range(r, regperiod_range("2010M1", "2011M11"))
    expect_identical(r, as.numeric(as.regperiod_range("2010M10/2011M11")))
    r <- regperiod_range(NULL, "2010")
    r <- regts:::convert_selection_range(r, regperiod_range("2010M2", "2011M11"))
    expect_identical(r, as.numeric(as.regperiod_range("2010M2/2010M12")))
})
