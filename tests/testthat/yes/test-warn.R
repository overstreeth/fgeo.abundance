context("warn")

test_that("is_suspicious_argument works as expected", {
  gx <- 0:3
  expect_silent(warn_suspicious(.x = substitute(gx < 0), msg = "Some "))
  gx <- 0:3
  expect_silent(warn_suspicious(.x = substitute(gx < 0)))
  gx <- -1:1
  expect_warning(warn_suspicious(.x = substitute(gx < 0), msg = "Some "),
    "Some gx < 0", fixed = TRUE)
  expect_warning(warn_suspicious(.x = substitute(gx < 0)))
})

test_that("warn_bad_arg_to_id_gxgy() does not warn when it doesn't"), {
  gx = 1
  gy = 1
  gridsize = 20
  plotdim = c(1000, 500)
  expect_silent(warn_bad_arg_to_id_gxgy(gx, gy, gridsize, plotdim))
})

test_that("warn_bad_arg_to_id_gxgy() with and without optional message", {
  gx = 99999
  gy = 99999
  gridsize = 99999
  plotdim = c(99999, 99999)
  expect_warning(warn_bad_arg_to_id_gxgy(gx, gy, gridsize, plotdim))
  expect_warning(warn_bad_arg_to_id_gxgy(gx, gy, gridsize, plotdim, 
    msg = "Whatch out!!!"))
})

test_that("warn_bad_arg_to_id_rowcol() is silent when is should", {
  .row = 1:3
  .col = 4:6
  gridsize = 20
  plotdim = c(1000, 500)
  expect_silent(warn_bad_arg_to_id_rowcol(.row, .col, gridsize, plotdim))
  expect_silent(warn_bad_arg_to_id_rowcol(.row, .col, gridsize, plotdim, 
    msg = "Hi "))
  .col = -2
})
  
test_that("warn_bad_arg_to_id_rowcol() warns when is should", {
  .row = -1:1
  .col = 4:6
  gridsize = 20
  plotdim = c(1000, 500)
  expect_warning(warn_bad_arg_to_id_rowcol(.row, .col, gridsize, 
    plotdim, msg = "Hi "))
  .row <- Inf
  expect_warning(warn_bad_arg_to_id_rowcol(.row, .col, gridsize, 
    plotdim))
})
