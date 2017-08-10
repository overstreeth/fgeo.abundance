
test_that("warn_bad_arg_to_id_gxgy() warns with bad arguments", {
  expect_warning(
    warn_bad_arg_to_id_gxgy(
      gx = -1:8,
      gy = 1:10,
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_warning(
    warn_bad_arg_to_id_gxgy(
      gx = NA,
      gy = -5:10,
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_warning(
    warn_bad_arg_to_id_gxgy(
      gx = 2000,
      gy = -5:10,
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
})
test_that("warn_bad_arg_to_id_gxgy() is silent with good arguments", {
  expect_silent(
    warn_bad_arg_to_id_gxgy(
      gx = 0:10,
      gy = 0:10,
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_silent(
    warn_bad_arg_to_id_gxgy(
      gx = 1:10,
      gy = 1:10,
      gridsize = 999,
      plotdim = c(987, 654)
    )
  )
})
