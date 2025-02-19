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

library(lintr)

# The most recent version of lintr contains a return linter.
if ("return_linter" %in% default_linters) {
  linters <- linters_with_defaults(return_linter = NULL)
}

lints <- lint_package("pkg", exclusions = exclusions, linters = linters)

if (length(lints) > 0) {
  cat("\nLinter detected style issues:\n")
  print(lints)
  stop("Linter failed. Please fix the above issues.")
} else {
  message("No linting issues found")
}
