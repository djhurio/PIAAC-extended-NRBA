# Make zip

# Reset
rm(list = ls())
gc()

x <- list.files(path = "results", pattern = "\\.(doc|xls)x$")

if (length(x) != 7L) stop("Should be 7 files")

zip::zip(
  zipfile = glue::glue("../Extended_NRBA_LVA_{Sys.Date()}.zip"),
  files = x,
  root = "results"
)
