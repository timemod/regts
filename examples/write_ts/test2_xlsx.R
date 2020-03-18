library(regts)

x <-regts(matrix(1:9, ncol = 3), start = "2010Q2",
          names = c("a", "b", "c"),
          labels = c("azalea", "boterbloem", "campanula"))
print(x)

write_ts_xlsx(x, file = "test2.xlsx", append = TRUE)

write_ts_xlsx(x, file = "test2_t.xlsx", append = TRUE,
              rowwise = FALSE)
