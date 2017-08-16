context("validate_to_id_rowcol")

test_that("from to_id_rowcol(), correctly calls functions that err or warn", {
  expect_warning(
    to_id_rowcol(.row = rep(NA_real_, 3), .col = 1:3, gridsize = 20, 
      plotdim = c(1000, 500))
  )
  expect_warning(
    to_id_rowcol(.row = 1:3, .col = rep(NA_real_, 3), gridsize = 20, 
      plotdim = c(1000, 500))
  )
  expect_error(
    to_id_rowcol(.row = 1:3, .col = 1:3, gridsize = NA_real_, 
      plotdim = c(1000, 500))
  )
  expect_error(
    to_id_rowcol(.row = 1:3, .col = 1:3, gridsize = 20, 
      plotdim = c(-1000, 500))
  )
})
