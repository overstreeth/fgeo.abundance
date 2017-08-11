context("validate_row_col")

test_that("validate_row_col() is silent when it should", {
  expect_silent(validate_row_col(.row = 3, .col = 0))
})
test_that("validate_row_col() warns if detects NA", {
  expect_warning(validate_row_col(.row = NA, .col = 0))
  expect_warning(validate_row_col(.row = NA, .col = NA))
})
