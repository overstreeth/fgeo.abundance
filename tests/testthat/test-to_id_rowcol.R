context("to_id_rowcol")

test_that("outputs the same as ctfs::rowcol.to.index", {
  some_vector <- 1:10
  before <- ctfs::rowcol.to.index(
    rowno = some_vector, 
    colno = some_vector,
    gridsize = 20,
    plotdim = c(1000, 500)
  )
  now <- to_id_rowcol(
    .row = some_vector, 
    .col = some_vector,
    gridsize = 20,
    plotdim = c(1000, 500)
  )
  expect_equal(before, now)
})

test_that("suspicious input throw warning or error", {
  vector_with_na <- c(1:9, NA_real_)
  expect_warning(
    to_id_rowcol(
      .row = vector_with_na, 
      .col = 1:10, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_warning(
    to_id_rowcol(
      .row = 1:10, 
      .col = vector_with_na, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  empty_numeric <- numeric(0)
  expect_error(
    to_id_rowcol(
      .row = 1:10, 
      .col = empty_numeric, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_error(
    to_id_rowcol(
      .row = empty_numeric, 
      .col = 1:10, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  vector_with_infinite <- c(1:9, Inf)
  expect_warning(
    to_id_rowcol(
      .row = vector_with_infinite, 
      .col = 1:10, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_warning(
    to_id_rowcol(
      .row = 1:10, 
      .col = vector_with_infinite, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
})

test_that("0 in gridsize or plotdim throws error",  {
  expect_error(
    to_id_rowcol(
      .row = 0:10, 
      .col = 0:10, 
      gridsize = 0,
      plotdim = c(1000, 500)
    )
  )
  expect_error(
    to_id_rowcol(
      .row = 0:10, 
      .col = 0:10, 
      gridsize = 20,
      plotdim = c(1000, 0)
    )
  )
})
