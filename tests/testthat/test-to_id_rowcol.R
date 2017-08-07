context("to_id_rowcol")

test_that("outputs the same as ctfs::rowcol.to.index", {
  some_vector <- 1:10
  before <- ctfs::rowcol.to.index(rowno = some_vector, colno = some_vector)
  now <- to_id_rowcol(rowno = some_vector, colno = some_vector)
  expect_equal(before, now)
})

test_that("suspicious input throw warning", {
  vector_with_na <- c(1:9, NA_real_)
  expect_warning(to_id_rowcol(vector_with_na, 1:10))
})
