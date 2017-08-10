context("validate_gridsize_plotdim")

test_that("is silent as expected", {
  expect_silent(
    validate_gridsize_plotdim(gridsize = 20, plotdim = c(1000, 500))
  )
  expect_silent(
    validate_gridsize_plotdim(gridsize = 1, plotdim = c(1, 1))
  )
})

test_that("errs as expected", {
  expect_error(
    validate_gridsize_plotdim(gridsize = 0, plotdim = c(1000, 500))
  )
  expect_error(
    validate_gridsize_plotdim(gridsize = 20, plotdim = c(0, 500))
  )
  expect_error(
    validate_gridsize_plotdim(gridsize = 20, plotdim = c(1000, 0))
  )
  expect_error(
    validate_gridsize_plotdim(gridsize = NA, plotdim = c(1000, 0))
  )
  expect_error(
    validate_gridsize_plotdim(gridsize = 0, plotdim = c(NA, 0))
  )
})
