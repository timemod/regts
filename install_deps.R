#!/usr/bin/Rscript

local({
  cran_repo <-  "https://cloud.r-project.org"
  r <- getOption("repos")
  r["CRAN"] <- cran_repo
  options("repos" = r)
})


user_lib_dir <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib_dir)) stopifnot(dir.create(user_lib_dir, recursive = TRUE))

if (!require(devtools)) {
   install.packages("devtools", lib = user_lib_dir)
}
if (!require(testthat)) {
   install.packages("devtools", lib = user_lib_dir)
}


devtools::install_deps("pkg", dependencies = TRUE, lib = user_lib_dir)
