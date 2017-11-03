bases <- c(
  "./",
  "./vignettes/",
  "./tests/testthat/"
)
lapply(bases, rm_files, file = "pdf$")
lapply(bases, rm_files, file = "png$")
