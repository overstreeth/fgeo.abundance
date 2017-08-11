context("basal_area")

# `ba()` ------------------------------------------------------------------

test_that("`ba()` outputs the same as original `ctfs::ba()`", {
  dbh <- c(1, 23, 43)
  expect_equal(ba(dbh = dbh), ctfs::ba(dbh = dbh))
})
test_that("in `ba()` dbhunit defaults to milimiters", {
  dbh <- c(1, 23, 43)
  expect_equal(forestr::ba(dbh = dbh), ba(dbh = dbh, dbhunit = "mm"))
  expect_false(identical(ba(dbh = dbh), ba(dbh = dbh, dbhunit = "cm")))
})
test_that("`ba()` errs with wrong dbhunit", {
  expect_error(ba(dbh, "wrong"))
})
test_that("`ba()` errs with wrong dbh", {
  expect_error(ba(c(0, 1, 2)))
  expect_warning(ba(dbh = c(NA, 1, 2, 3)))
})



# `basum()` ---------------------------------------------------------------

test_that("`basum()` outputs the same as original `ctfs::basum()`", {
  dbh <- c(1, 23, 43)
  expect_equal(
    forestr::basum(dbh = dbh, mindbh = 10), 
    ctfs::basum(dbh = dbh, mindbh = 10)
  )
  expect_equal(
    forestr::basum(dbh = dbh, mindbh = 23, dbhunit = "cm"), 
    ctfs::basum(dbh = dbh, mindbh = 23, dbhunit = "cm")
  )
})
test_that("in `basum()` dbhunit defaults to milimiters", {
  dbh <- c(1, 23, 43)
  expect_equal(
    basum(dbh = dbh, mindbh = 10), 
    basum(dbh = dbh, mindbh = 10, dbhunit = "mm")
  )
  expect_false(
    identical(
      basum(dbh = dbh, mindbh = 10), 
      basum(dbh = dbh, mindbh = 10, dbhunit = "cm")
    )
  )
})
test_that("`basum()` errs with wrong arguments", {
  expect_error(basum("wrong", mindbh = 10, dbhunit = "cm"))
  expect_error(basum(dbh, mindbh = 10, dbhunit = "wrong"))
  expect_error(basum(dbh, mindbh = "wrong"))
  expect_error(basum(c(0, 1, 2)))
})
test_that("`basum()` warns with NAs in dbh", {
  expect_warning(basum(dbh = c(NA, 1, 2, 3)))
})
