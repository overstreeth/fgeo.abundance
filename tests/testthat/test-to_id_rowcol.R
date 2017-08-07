context("to_id_rowcol")

test_that("outputs the same as ctfs::rowcol.to.index", {
  some_vector <- 1:10
  before <- ctfs::rowcol.to.index(rowno = some_vector, colno = some_vector)
  now <- to_id_rowcol(rowno = some_vector, colno = some_vector)
  expect_equal(before, now)
})
