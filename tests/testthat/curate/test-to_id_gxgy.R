context("to_id_gxgy")

gx <- c(747.2, 301.19, 864.8)
gy <- c(167.8, 397.1, 100.9)
gridsize <- 20
plotdim <- c(1000, 500)

test_that("outputs the same as ctfs::rowcol.to.index", {
  before <- ctfs::gxgy.to.index(gx, gy, gridsize, plotdim)
  now <- to_id_gxgy(gx, gy, gridsize, plotdim)
  expect_equal(before, now)

  gx_na <- c(747.2, 301.19, NA)
  before <- ctfs::gxgy.to.index(gx_na, gy, gridsize, plotdim)
  expect_warning(
    now <- to_id_gxgy(gx_na, gy, gridsize, plotdim)
  )
  expect_equal(before, now)
})

thest_that("warnings occurr as expected", {
  gy_na <- c(gy, NA)
  expect_warning(to_id_gxgy(gx, gy_na, gridsize, plotdim))
  gx_negative <- c(gx, -1)
  expect_warning(to_id_gxgy(gx_negative, gy_na, gridsize, plotdim))
})

test_that("errors occurr as expected", {
  gridsize_negative <- -10
  expect_error(to_id_gxgy(gx, gy, gridsize_negative, plotdim))
  plotdim_negative <- -10
  expect_error(to_id_gxgy(gx, gy, gridsize, plotdim_negative))
})

test_that("type is correct", {
  result <- to_id_gxgy(gx, gy, gridsize, plotdim)
  expect_type(result, "double")
})
