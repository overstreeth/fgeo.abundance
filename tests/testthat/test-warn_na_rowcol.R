context("warn_na_rowcol")

test_that("warn_na_rowcol() is silent when it should", {
  expect_silent(warn_na_rowcol(.row = 3, .col = 0))
})
test_that("warn_na_rowcol() warns if detects NA", {
  expect_warning(warn_na_rowcol(.row = NA, .col = 0))
  expect_warning(warn_na_rowcol(.row = NA, .col = NA))
})
