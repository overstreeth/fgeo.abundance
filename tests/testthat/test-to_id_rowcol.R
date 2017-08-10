context("to_id_rowcol")

.row <- 1:3
.col <- 1:3
gridsize <- 20
plotdim <- c(1000, 500)

test_that("outputs the same as ctfs::rowcol.to.index", {
  before <- ctfs::rowcol.to.index(.row, .col, gridsize, plotdim)
  now <- to_id_rowcol(.row, .col, gridsize, plotdim)
  expect_equal(before, now)

  .row_na <- c(.row, NA)
  expect_warning(
    before <- ctfs::rowcol.to.index(.row_na, .col, gridsize, plotdim)
  )
  expect_warning(
    now <- to_id_rowcol(.row_na, .col, gridsize, plotdim)
  )
  expect_equal(before, now)
})

test_that("warnings occurr as expected", {
  .col_na <- c(.col, NA)
  expect_warning(to_id_rowcol(.row, .col_na, gridsize, plotdim))
  .row_negative <- c(.row, -1)
  expect_warning(to_id_rowcol(.row_negative, .col_na, gridsize, plotdim))
})

test_that("errors occurr as expected", {
  gridsize_negative <- -10
  expect_error(to_id_rowcol(.row, .col, gridsize_negative, plotdim))
  plotdim_negative <- -10
  expect_error(to_id_rowcol(.row, .col, gridsize, plotdim_negative))
})

test_that("type is correct", {
  result <- to_id_rowcol(.row, .col, gridsize, plotdim)
  expect_type(result, "double")
})
