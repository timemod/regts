# Exclude the following files from lintr. These files still contain many
# style errors. The style of these function should be fixed once these function
# are fixed.
exclusions <- list(
  "tests",
  "vignettes",
  "R/cbind.regts.R",
  "R/index_ts.R",
  "R/join_ts.R",
  "R/methods.R",
  "R/Ops.regts.R",
  "R/period_range.R",
  "R/period.R",
  "R/print.R",
  "R/RcppExports.R",
  "R/read_utils.R",
  "R/regts.R",
  "R/seq.R",
  "R/update_ts.R",
  "R/write_ts.R"
)

lintr::lint_package("pkg", exclusions = exclusions)
