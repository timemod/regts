context("regperiod_range_utils")

test_that(".regrange_intersect", {
    r1 <- as.regperiod_range("2010Q4/2011Q3")
    r2 <- as.regperiod_range("2011Q1/2012Q4")
    expect_identical(as.character(regrange_intersect(r1, r1)), "2010Q4/2011Q3")
    expect_identical(as.character(regrange_intersect(r1, r2)), "2011Q1/2011Q3")
    expect_identical(as.character(regrange_intersect(r2, r1)), "2011Q1/2011Q3")

    r2 <- regperiod_range("2008Q1", "2011Q2")
    expect_identical(as.character(regrange_intersect(r1, r2)), "2010Q4/2011Q2")
    expect_identical(as.character(regrange_intersect(r2, r1)), "2010Q4/2011Q2")

    r2 <- regperiod_range("2019Q2", "2020Q2")
    expect_null(regrange_intersect(r1, r2))
    expect_null(regrange_intersect(r2, r1))
})

test_that(".regrange_union", {
    r1 <- as.regperiod_range("2010Q4/2011Q3")
    r2 <- as.regperiod_range("2011Q1/2012Q4")
    expect_identical(as.character(regrange_union(r1, r1)), "2010Q4/2011Q3")
    expect_identical(as.character(regrange_union(r1, r2)), "2010Q4/2012Q4")
    expect_identical(as.character(regrange_union(r2, r1)), "2010Q4/2012Q4")

    r2 <- regperiod_range("2008Q1", "2011Q2")
    expect_identical(as.character(regrange_union(r1, r2)), "2008Q1/2011Q3")
    expect_identical(as.character(regrange_union(r2, r1)), "2008Q1/2011Q3")

    r2 <- regperiod_range("2019Q2", "2020Q2")
    expect_identical(as.character(regrange_union(r1, r2)), "2010Q4/2020Q2")
    expect_identical(as.character(regrange_union(r2, r1)), "2010Q4/2020Q2")


})

test_that(".regrange_errors", {
    r1 <- regperiod_range("2010Q4", "2011Q3")
    r2 <- regperiod_range("2010Q4", NULL)

    expect_error(regrange_union(r1,r2),
                 "Start and end periods of both ranges should not be NULL")
    r1 <- regperiod_range(NULL, "2011Q3")
    expect_error(regrange_intersect(r1,r2),
                 "Start and end periods of both ranges should not be NULL")

    r1 <- regperiod_range("2010Q4", "2011Q3")
    r3 <- as.regperiod_range("2010M4/2011M3")
    expect_error(regrange_union(r1,r3),
                 "The two periods have difffffferent frequency")
})
