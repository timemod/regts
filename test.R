#!/usr/bin/Rscript

if (!require(devtools)) {
  stop('devtools not installed')
}
devtools::test("pkg", stop_on_failure = TRUE)
