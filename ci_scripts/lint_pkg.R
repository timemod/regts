# Exclude the following files from lintr. These files still contain many
# style errors. The style of these function should be fixed later.
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
  "R/seq.R",
  "R/update_ts.R",
  "R/write_ts.R"
)

library(lintr)

# Create a list of linters that should be modified or removed ------------------
linters_mut <- list(
  object_name_linter = NULL,
  cyclocomp_linter = NULL,
  commented_code_linter = NULL,
  return_linter = NULL
)

# Some linters may not be available for the specific version of lintr.
sel <- names(linters_mut) %in% names(default_linters)
linters_mut <- linters_mut[sel]

#  Lint the files in the package -----------------------------------------------
linters <- do.call(linters_with_defaults, linters_mut)

# Some linters may not be available for the specific version of lintr.
sel <- names(linters_mut) %in% names(default_linters)
linters_mut <- linters_mut[sel]

#  Lint the files in the package -----------------------------------------------
linters <- do.call(linters_with_defaults, linters_mut)

lints <- lint_package("pkg", exclusions = exclusions, linters = linters)

if (length(lints) > 0) {
  cat("\nLinter detected style issues:\n")
  print(lints)
  stop("Linter failed. Please fix the above issues.")
} else {
  message("No linting issues found")
}
