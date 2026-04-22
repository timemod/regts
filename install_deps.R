#!/usr/bin/Rscript
repo <- "https://cloud.r-project.org"

options(repos = c(CRAN = repo)) 

# If the user library does not exist, then create it
# and add it to the library path.
user_lib_dir <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib_dir)) {
  dir.create(user_lib_dir, recursive = TRUE)
  .libPaths(user_lib_dir)
}

if (!require("remotes", character.only = TRUE, quietly = TRUE)) {
  install.packages("remotes", dependencies = FALSE)
} else {
  unloadNamespace("remotes")

}

# Install extra packages needed to install isismdl with the install script.
extra_packages <- c("devtools", "tictoc")
for (extra_package in extra_packages) {
  if (!require(extra_package, character.only = TRUE, quietly = TRUE)) {
    remotes::install_cran(extra_package, upgrade = "never")
  } else {
    unloadNamespace(extra_package)
  }
}

remotes::install_deps("pkg", dependencies = TRUE, upgrade = "never")
