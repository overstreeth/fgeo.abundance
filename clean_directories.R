bases <- c(
  "./",
  "./inst/tutorials/",
  "./tests/testthat/"
)
lapply(bases, rm_files, file = "pdf$")
lapply(bases, rm_files, file = "png$")
lapply(bases, rm_files, file = "html$")
