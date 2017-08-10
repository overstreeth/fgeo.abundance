context("basal_area")

test_that("outputs the same as original `ctfs::ba()`", {
  dbh <- c(1, 23, 43)
  expect_equal(ba(dbh = dbh), ctfs::ba(dbh = dbh))
})
test_that("dbhunit defaults to milimiters", {
  dbh <- c(1, 23, 43)
  expect_equal(forestr::ba(dbh = dbh), ba(dbh = dbh, dbhunit = "mm"))
  expect_false(identical(ba(dbh = dbh), ba(dbh = dbh, dbhunit = "cm")))
})
test_that("errs with wrong dbhunit", {
  expect_error(ba(dbh, "wrong"))
})
test_that("errs with wrong dbh", {
  expect_error(ba(c(0, 1, 2)))
  expect_warning(ba(dbh = c(NA, 1, 2)))
})
