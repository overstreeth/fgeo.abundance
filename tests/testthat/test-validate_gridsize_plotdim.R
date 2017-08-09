context("validate_gridsize_plotdim")

test_that("stops as expected", {
  expect_silent(
    validate_gridsize_plotdim(gridsize = 20, plotdim = c(1000, 500))
  )
  expect_silent(
    validate_gridsize_plotdim(gridsize = 1, plotdim = c(1, 1))
  )
  expect_error(
    validate_gridsize_plotdim(gridsize = 0, plotdim = c(1000, 500))
  )
  expect_error(
    validate_gridsize_plotdim(gridsize = 20, plotdim = c(0, 500))
  )
  expect_error(
    validate_gridsize_plotdim(gridsize = 20, plotdim = c(1000, 0))
  )
})
