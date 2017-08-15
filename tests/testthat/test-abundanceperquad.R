context("abundanceperquad")

library(dplyr)
cns5_mini <- sample_n(bci::bci12full5, 100)

old <- ctfs::abundanceperquad(
  cns5_mini,
  plotdim = c(1000, 500),
  gridsize = 100,
  type = "abund"
)
new <- forestr::abundanceperquad(
  cns5_mini,
  plotdim = c(1000, 500),
  gridsize = 100,
  type = "abund"
)

test_that("returns the same as ctfs::abundanceperquad", {
  expect_equal(old, new)
})

test_that("errs with wrong input", {
  expect_equal(old, new)
})






