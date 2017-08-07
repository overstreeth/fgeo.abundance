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
    rowno = some_vector, 
    colno = some_vector,
    gridsize = 20,
    plotdim = c(1000, 500)
  )
  expect_equal(before, now)
})

test_that("suspicious input throw warning", {
  vector_with_na <- c(1:9, NA_real_)
  expect_warning(
    to_id_rowcol(
      rowno = vector_with_na, 
      colno = 1:10, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_warning(
    to_id_rowcol(
      rowno = 1:10, 
      colno = vector_with_na, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  empty_numeric <- numeric(0)
  expect_error(
    to_id_rowcol(
      rowno = 1:10, 
      colno = empty_numeric, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_error(
    to_id_rowcol(
      rowno = empty_numeric, 
      colno = 1:10, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  vector_with_infinite <- c(1:9, Inf)
  expect_warning(
    to_id_rowcol(
      rowno = vector_with_infinite, 
      colno = 1:10, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_warning(
    to_id_rowcol(
      rowno = 1:10, 
      colno = vector_with_infinite, 
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
})
