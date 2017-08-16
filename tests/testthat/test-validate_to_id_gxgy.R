context("validate_to_id_gxgy")

test_that("from to_id_gxgy(), correctly calls functions that err or warn", {
  expect_warning(
    to_id_gxgy(gx = -1:1, gy = 1:3, gridsize = 20, 
      plotdim = c(1000, 500))
  )
  expect_warning(
    to_id_gxgy(gx = 1:3, gy = -1:1, gridsize = 20, 
      plotdim = c(1000, 500))
  )
  expect_error(
    to_id_gxgy(gx = 1:3, gy = 1:3, gridsize = rep(NA_real_, 3),
      plotdim = c(1000, 500))
  )
  expect_error(
    to_id_gxgy(gx = 1:3, gy = 1:3, gridsize = 20,
      plotdim = c(1000, -500))
  )
})
